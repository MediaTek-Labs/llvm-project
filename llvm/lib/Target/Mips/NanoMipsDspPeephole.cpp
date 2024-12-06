//===--------- NanoMipsDspPeephole.cpp - nanoMIPS peephole pass ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file This file contains a pass that performs DSP related peephole
/// optimizations. This pass should be run before register allocation.
//
//===----------------------------------------------------------------------===//

#include "Mips.h"
#include "MipsSubtarget.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineBlockFrequencyInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/InitializePasses.h"

using namespace llvm;

#define PASS_NAME "nanoMIPS DSP Peephole pass"
#define DEBUG_TYPE "nm-dsp-peep"
static cl::opt<bool>
    DisableDspPeeps("disable-nm-dsp-peep",
                    cl::desc("Disable Nanomips DSP peephole pass"),
                    cl::init(false));
namespace {

typedef enum { partLow, partHigh } Part;

namespace acc {
constexpr unsigned ExtractLo = Mips::MFLO_NM;
constexpr unsigned ExtractHi = Mips::MFHI_NM;
unsigned Extract(Part P) {
  return P == partLow ? Mips::MFLO_NM : Mips::MFHI_NM;
}
constexpr unsigned Compose = Mips::PseudoMTLOHI_DSP_NM;
const TargetRegisterClass *RC = &Mips::ACC64DSPNMRegClass;
} // namespace acc

typedef std::pair<MachineInstr *, MachineInstr *> MIPair;
typedef std::pair<MachineOperand, MachineOperand> MOPair;
typedef SmallVectorImpl<MachineInstr *> VecMI;

struct NMDspPeephole : public MachineFunctionPass {
  static char ID;
  const MipsSubtarget *STI;
  const TargetRegisterInfo *TRI;
  MachineBlockFrequencyInfo *MBFI;
  MachineRegisterInfo *MRI;
  SmallSet<MachineInstr *, 8> ConvertedPHIs;
  SmallSet<MachineInstr *, 8> VisitedPHIs;
  DenseMap<MachineInstr *, MachineInstr *> ChangedPHIMap;
  MachineInstr *RootLoPHI = nullptr;
  MachineInstr *RootHiPHI = nullptr;
  NMDspPeephole() : MachineFunctionPass(ID) {}
  bool usedByComposeOnly(MachineInstr *);
  bool isRoot(MachineInstr *, Part);
  bool requiresExtract(MachineInstr *, Part);
  bool requiresCompose(const MachineOperand &, Part P);
  void BuildExtract(MachineInstr *, Register, Part);
  MachineInstr *getExtract(const MachineOperand &, Part);
  bool isUsedElsewhere(MachineInstr *, VecMI &, unsigned);
  void accConversionBeneficial(MachineInstr *, VecMI &, Part, uint64_t &,
                               uint64_t &);
  StringRef getPassName() const override { return PASS_NAME; }
  Register getAccRegOrNone(MachineOperand &MO);
  MachineBasicBlock::iterator afterLastPhi(MachineBasicBlock::iterator);
  bool collectFeedingAccPHIs(MachineInstr *, VecMI &);
  void convertFeedingAccPHIs(SmallVectorImpl<MIPair> &);
  void recordPHIAccOps(MachineInstr *, unsigned, Part,
                       SmallVectorImpl<MOPair> &, MachineInstrBuilder &, bool);
  bool matchingPHIs(MachineInstr *, MachineInstr *);
  MachineInstr *findExtractPair(MachineInstr &);
  bool convertPHIsGPRToAcc(MachineBasicBlock &);
  bool compareAndZipPHIVec(const VecMI &, const VecMI &,
                           SmallVectorImpl<MIPair> &);
  bool LLVM_ATTRIBUTE_UNUSED recordedPair(MOPair &, SmallVectorImpl<MOPair> &);
  SmallVector<MIPair, 4> compareAndZipPHIVec(const VecMI &, const VecMI &);
  bool removeRedundantMoveFromAcc(MachineBasicBlock &);
  MachineInstr *getPHIUsage(MachineInstr &);
  MachineInstr *getMIDefiningReg(Register, unsigned);
  Register firstAccOperand(MachineInstr &MI);
  void replaceRegUsesWith(Register FromReg, Register ToReg);
  bool runOnMachineFunction(MachineFunction &Fn) override;
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<MachineBlockFrequencyInfoWrapperPass>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
};
} // namespace

char NMDspPeephole::ID = 0;
INITIALIZE_PASS_BEGIN(NMDspPeephole, DEBUG_TYPE, PASS_NAME, false, false)
INITIALIZE_PASS_DEPENDENCY(MachineBlockFrequencyInfoWrapperPass)
INITIALIZE_PASS_END(NMDspPeephole, DEBUG_TYPE, PASS_NAME, false, false)
 

// Adapted version of [PowerPC] Fix for excessive ACC copies due to PHI nodes
// (PPCMIPeephole.cpp)
// This function returns a list of all PHI nodes in the tree starting from
// the RootPHI node. We perform a BFS traversal to get an ordered list of nodes.
// The list initially only contains the root PHI. When we visit a PHI node, we
// add it to the list. We continue to look for other PHI node operands while
// there are nodes to visit in the list. The function returns false if the
// optimization cannot be applied on this tree.
bool NMDspPeephole::collectFeedingAccPHIs(MachineInstr *RootPHI, VecMI &PHIs) {
  PHIs.push_back(RootPHI);
  unsigned VisitedIndex = 0;
  while (VisitedIndex < PHIs.size()) {
    MachineInstr *VisitedPHI = PHIs[VisitedIndex];
    for (unsigned PHIOp = 1, NumOps = VisitedPHI->getNumOperands();
         PHIOp != NumOps; PHIOp += 2) {
      Register RegOp = VisitedPHI->getOperand(PHIOp).getReg();
      if (!RegOp.isVirtual())
        return false;
      MachineInstr *Instr = MRI->getVRegDef(RegOp);
      if (Instr->getOpcode() != Mips::PHI)
        continue;
      if (llvm::is_contained(PHIs, Instr))
        return false;
      PHIs.push_back(Instr);
    }
    VisitedIndex++;
    // too much
    if (VisitedIndex > 100)
      return false;
  }
  return true;
}

bool NMDspPeephole::compareAndZipPHIVec(const VecMI &PHILo, const VecMI &PHIHi,
                                        SmallVectorImpl<MIPair> &Zipped) {
  if (PHILo.size() != PHIHi.size())
    return false;
  for (size_t i = 0; i < PHILo.size(); ++i) {
    if (!matchingPHIs(PHILo[i], PHIHi[i]))
      return false;
    Zipped.push_back(std::make_pair(PHILo[i], PHIHi[i]));
  }
  return true;
}

bool NMDspPeephole::matchingPHIs(MachineInstr *PHILo, MachineInstr *PHIHi) {
  if (PHILo->getNumOperands() != PHIHi->getNumOperands())
    return false;
  if (PHILo->getParent() != PHIHi->getParent())
    return false;
  for (unsigned i = 1, NumOps = PHILo->getNumOperands(); i < NumOps; i += 2) {
    MachineInstr *InstrLo = MRI->getVRegDef(PHILo->getOperand(i).getReg());
    MachineInstr *InstrHi = MRI->getVRegDef(PHIHi->getOperand(i).getReg());
    bool OpLoExtract = InstrLo && InstrLo->getOpcode() == acc::ExtractLo;
    bool OpHiExtract = InstrHi && InstrHi->getOpcode() == acc::ExtractHi;
    if (OpLoExtract != OpHiExtract)
      return false;
    bool OpLoPhi = InstrLo && InstrLo->getOpcode() == Mips::PHI;
    bool OpHiPhi = InstrHi && InstrHi->getOpcode() == Mips::PHI;
    if (OpLoPhi != OpHiPhi)
      return false;
    if (OpLoExtract && MRI->getVRegDef(InstrLo->getOperand(1).getReg()) !=
                           MRI->getVRegDef(InstrHi->getOperand(1).getReg()))
      return false;
    if (PHILo->getOperand(i + 1).getMBB() != PHIHi->getOperand(i + 1).getMBB())
      return false;
  }
  return true;
}

bool NMDspPeephole::usedByComposeOnly(MachineInstr *MI) {
  Register Reg = MI->getOperand(0).getReg();
  for (MachineInstr &UseMI : MRI->use_nodbg_instructions(Reg))
    if (UseMI.getOpcode() != acc::Compose)
      return false;
  return true;
}

bool NMDspPeephole::isRoot(MachineInstr *MI, Part P) {
  return MI == (P == partLow ? RootLoPHI : RootHiPHI);
}

bool NMDspPeephole::requiresExtract(MachineInstr *MI, Part P) {
  return isRoot(MI, P) &&
         !usedByComposeOnly(P == partLow ? RootLoPHI : RootHiPHI);
}

bool NMDspPeephole::requiresCompose(const MachineOperand &MO, Part P) {
  Register RegOp = MO.getReg();
  MachineInstr *PHIInput = MRI->getVRegDef(RegOp);
  unsigned Opcode = PHIInput->getOpcode();
  if (Opcode == acc::Extract(P) || Opcode == Mips::PHI) {
    return false;
  }
  return true;
}

bool NMDspPeephole::recordedPair(MOPair &P, SmallVectorImpl<MOPair> &PHIOps) {
  for (auto I : PHIOps) {
    if (I.first.isIdenticalTo(P.first) && I.second.isIdenticalTo(P.second)) {
      return true;
    }
  }
  return false;
}

// This function is called twice, for Low and High operands
// Since two GPR PHIs are replaced with a single ACC PHI,
// The work to be done is asymmetric. The Low traversal does:
// - storing operands for the new PHI
// - creating ACC compose if needed
// The High traversal just adds the High operand to the ACC compose
void NMDspPeephole::recordPHIAccOps(MachineInstr *PHI, unsigned PHIOp, Part P,
                                    SmallVectorImpl<MOPair> &PHIOps,
                                    MachineInstrBuilder &AccCompose,
                                    bool buildCompose) {
  Register RegOp = PHI->getOperand(PHIOp).getReg();
  const MachineOperand SrcLabel = PHI->getOperand(PHIOp + 1);
  MachineInstr *PHIInput = MRI->getVRegDef(RegOp);
  unsigned Opcode = PHIInput->getOpcode();
  if (buildCompose) {
    // This is a GPR source that we need to convert to ACC
    if (P == partLow) {
      auto *SrcBB = SrcLabel.getMBB();
      auto Pos = SrcBB->getFirstTerminator();
      // At Low traversal - create the conversion Instr
      Register AccReg = MRI->createVirtualRegister(acc::RC);
      const MipsInstrInfo *TII = STI->getInstrInfo();
      AccCompose = BuildMI(*SrcBB, Pos, PHIInput->getDebugLoc(),
                           TII->get(acc::Compose), AccReg)
                       .add(PHI->getOperand(PHIOp));
      PHIOps.push_back({AccCompose.getInstr()->getOperand(0), SrcLabel});
    } else {
      // At High traversal - just add the missing operand
      AccCompose.add(PHI->getOperand(PHIOp));
    }
  } else if (Opcode == acc::Extract(P)) {
    MOPair Ops = std::make_pair(PHIInput->getOperand(1), SrcLabel);
    if (P == partLow)
      PHIOps.push_back(Ops);
    else
      assert(recordedPair(Ops, PHIOps));
  } else if (Opcode == Mips::PHI) {
    // We found a PHI operand. At this point we know this operand
    // has already been changed so we get its associated changed from
    // from the map.
    if (P == partLow) {
      assert(ChangedPHIMap.count(PHIInput) == 1 &&
             "This PHI node should have already been changed.");
      MachineInstr *AccPHI = ChangedPHIMap.lookup(PHIInput);
      PHIOps.push_back(
          {MachineOperand::CreateReg(AccPHI->getOperand(0).getReg(), false),
           SrcLabel});
    }
  } else
    llvm_unreachable("Unexpected instruction");
}

MachineInstr *NMDspPeephole::getExtract(const MachineOperand &MO, Part P) {
  Register RegOp = MO.getReg();
  if (RegOp.isVirtual()) {
    MachineInstr *Instr = MRI->getVRegDef(RegOp);
    unsigned Opcode = Instr->getOpcode();
    if (Opcode == acc::Extract(P))
      return Instr;
  }
  return nullptr;
}

bool NMDspPeephole::isUsedElsewhere(MachineInstr *Def, VecMI &CandidatePHIs,
                                    unsigned Reg) {
  // Iterate over all uses of the register
  for (MachineInstr &UseMI : MRI->use_nodbg_instructions(Reg)) {
    // If any use is out of the  optimized instructions, return false
    if (&UseMI != Def && find(CandidatePHIs, &UseMI) == CandidatePHIs.end() &&
        find(ConvertedPHIs, &UseMI) == ConvertedPHIs.end()) {
      return true;
    }
  }
  return false;
}

// greedy cost of converting PHI from GPR to ACC
void NMDspPeephole::accConversionBeneficial(MachineInstr *PHI, VecMI &PHIs,
                                            Part P, uint64_t &CostBaseline,
                                            uint64_t &CostChange) {

  auto BBFrequency = [this](const MachineBasicBlock *BB) {
    BlockFrequency Freq = MBFI->getBlockFreq(BB);
    return Freq.getFrequency();
  };

  // Iterate over the operands of the PHI node
  // to calculate cost of moving ACC extract
  for (unsigned i = 1; i < PHI->getNumOperands(); i += 2) {
    const MachineOperand &MO = PHI->getOperand(i);
    const MachineOperand &BB = PHI->getOperand(i + 1);
    if (BB.isMBB()) {
      if (MachineInstr *Extract = getExtract(MO, P)) {
        // add to baseline cost if this extract would be redundant
        // if there are uses other than that phi, it might not be removed
        if (!isUsedElsewhere(Extract, PHIs, MO.getReg())) {
          uint64_t ExecutionCount = BBFrequency(Extract->getParent());
          CostBaseline += ExecutionCount;
        }
      } else if (requiresCompose(MO, P)) {
        uint64_t ExecutionCount = BBFrequency(BB.getMBB());
        CostChange += ExecutionCount;
      }
    }
  }

  // Calculating cost of moving ACC compose
  unsigned Reg = PHI->getOperand(0).getReg();
  for (MachineInstr &MUI : MRI->use_nodbg_instructions(Reg)) {
    if (MUI.getOpcode() == acc::Compose &&
        MUI.getOperand(P == partLow ? 1 : 2).getReg() == Reg) {
      CostBaseline += BBFrequency(MUI.getParent());
    }
  }
  // Missing Extract will be added next to the PHI
  if (requiresExtract(PHI, P))
    CostChange += BBFrequency(PHI->getParent());
}

MachineBasicBlock::iterator
NMDspPeephole::afterLastPhi(MachineBasicBlock::iterator I) {
  // Find the last PHI instruction
  MachineBasicBlock *MBB = I->getParent();
  auto LastPHI = MBB->end();
  for (auto It = I, E = MBB->end(); It != E; ++It)
    if (It->isPHI())
      LastPHI = It;
  // If there was at least one PHI instruction,
  // increment the iterator to point after the last PHI
  if (LastPHI != MBB->end())
    return ++LastPHI;
  else
    return I;
}

void NMDspPeephole::BuildExtract(MachineInstr *PHI, Register AccReg, Part P) {
  Register ResultOrig = PHI->getOperand(0).getReg();
  const TargetRegisterClass *VirtRegClass = MRI->getRegClass(ResultOrig);
  Register Result = MRI->createVirtualRegister(VirtRegClass);
  replaceRegUsesWith(ResultOrig, Result);
  const MipsInstrInfo *TII = STI->getInstrInfo();
  // Extract ACC is placed at PHI location
  // TODO: reconsider, based on performance
  auto InsertPoint = afterLastPhi(std::next(PHI->getIterator()));
  BuildMI(*PHI->getParent(), InsertPoint, PHI->getDebugLoc(),
          TII->get(acc::Extract(P)), Result)
      .addReg(AccReg);
}

// This function changes each pair of GPR PHIs nodes in the PHIs list to
// accumulator PHI nodes. The list is traversed in reverse order to
// change all the PHI operands of a PHI node before changing the node itself.
// We keep a map to associate each changed PHI node to its non-changed form.
void NMDspPeephole::convertFeedingAccPHIs(SmallVectorImpl<MIPair> &PHIs) {
  for (const auto &Pair : llvm::reverse(PHIs)) {
    MachineInstr *PHILo = Pair.first;
    MachineInstr *PHIHi = Pair.second;
    if (ChangedPHIMap[PHILo])
      continue;
    SmallVector<MOPair, 4> PHIOps;
    // record in PHIOps the accumulator and its source block
    for (unsigned PHIOp = 1, NumOps = PHILo->getNumOperands(); PHIOp != NumOps;
         PHIOp += 2) {
      // create an accumulator compose if needed
      bool buildCompose = requiresCompose(PHILo->getOperand(PHIOp), partLow) ||
                          requiresCompose(PHIHi->getOperand(PHIOp), partHigh);
      MachineInstrBuilder AccCompose;
      recordPHIAccOps(PHILo, PHIOp, partLow, PHIOps, AccCompose, buildCompose);
      recordPHIAccOps(PHIHi, PHIOp, partHigh, PHIOps, AccCompose, buildCompose);
    }
    Register AccReg = MRI->createVirtualRegister(acc::RC);

    // replace the result of Compose with a new ACC
    // doing it once for Low part, since there is a single Compose result
    // for both Low and High
    if (isRoot(PHILo, partLow)) {
      Register RegGPR = PHILo->getOperand(0).getReg();
      for (MachineInstr &UseMI : MRI->use_nodbg_instructions(RegGPR))
        if (UseMI.getOpcode() == acc::Compose)
          replaceRegUsesWith(UseMI.getOperand(0).getReg(), AccReg);
    }
    // New ACC PHI
    const MipsInstrInfo *TII = STI->getInstrInfo();
    MachineInstrBuilder NewPHI =
        BuildMI(*(PHILo->getParent()), PHILo->getIterator(),
                PHILo->getDebugLoc(), TII->get(Mips::PHI), AccReg);
    for (auto RegMBB : PHIOps)
      NewPHI.addReg(RegMBB.first.getReg()).add(RegMBB.second);
    ChangedPHIMap[PHILo] = NewPHI.getInstr();

    // Extract from PHI result back to GPR consumers
    if (requiresExtract(PHILo, partLow))
      BuildExtract(PHILo, AccReg, partLow);
    if (requiresExtract(PHIHi, partHigh))
      BuildExtract(PHIHi, AccReg, partHigh);

    LLVM_DEBUG(dbgs() << "Converting PHIs: ");
    LLVM_DEBUG(PHILo->dump());
    LLVM_DEBUG(PHIHi->dump());
    LLVM_DEBUG(dbgs() << "To: ");
    LLVM_DEBUG(NewPHI.getInstr()->dump());
  }
}

MachineInstr *NMDspPeephole::getMIDefiningReg(Register Reg, unsigned Opcode) {
  if (Reg.isVirtual()) {
    MachineInstr *DefMI = MRI->getVRegDef(Reg);
    if (DefMI && DefMI->getOpcode() == Opcode)
      return DefMI;
  }
  return nullptr;
}

Register NMDspPeephole::getAccRegOrNone(MachineOperand &MO) {
  if (MO.isReg()) {
    Register Reg = MO.getReg();
    if (Reg.isVirtual())
      if (MRI->getRegClass(Reg) == acc::RC)
        return Reg;
  }
  return Mips::NoRegister;
}

Register NMDspPeephole::firstAccOperand(MachineInstr &MI) {
  for (MachineOperand &MO : MI.operands()) {
    if (!MO.isReg() || MO.isDef())
      continue;
    Register Reg = getAccRegOrNone(MO);
    if (Reg != Mips::NoRegister)
      return Reg;
  }
  return Mips::NoRegister;
}

// find first PHI usage that has not been visited yet
// if there are more, we will get them next time around
MachineInstr *NMDspPeephole::getPHIUsage(MachineInstr &MI) {
  unsigned Reg = MI.getOperand(0).getReg();
  for (MachineInstr &MUI : MRI->use_nodbg_instructions(Reg)) {
    if (MUI.getOpcode() == Mips::PHI &&
        find(VisitedPHIs, &MUI) == VisitedPHIs.end()) {
      return &MUI;
    }
  }
  return nullptr;
}

MachineInstr *NMDspPeephole::findExtractPair(MachineInstr &MI) {
  assert(MI.getOpcode() == acc::ExtractHi);
  unsigned ExtractOpcode = acc::ExtractLo;
  unsigned Reg = MI.getOperand(1).getReg();
  for (MachineInstr &MUI : MRI->use_instructions(Reg))
    if (MUI.getOpcode() == ExtractOpcode && MUI.getParent() == MI.getParent())
      return &MUI;
  return nullptr;
}

// convert GPR PHIs to accumulator PHIs
bool NMDspPeephole::convertPHIsGPRToAcc(MachineBasicBlock &MBB) {
  bool Modified = false;
  llvm::MachineBasicBlock::iterator Next;
  for (MachineBasicBlock::iterator I = MBB.begin(), E = MBB.end(); I != E;
       I = Next) {
    Next = std::next(I);
    MachineInstr &MI = *I;
    RootLoPHI = nullptr;
    RootHiPHI = nullptr;
    bool ExtractRoot = false;

    // TODO : skip optimization if all operands or all result usage are GPRs
    switch (MI.getOpcode()) {
    // Detect candidates PHI whose usage is ACC
    case acc::Compose: {
      RootLoPHI = MRI->getVRegDef(MI.getOperand(1).getReg());
      RootHiPHI = MRI->getVRegDef(MI.getOperand(2).getReg());
      if (RootLoPHI->getOpcode() != Mips::PHI ||
          RootHiPHI->getOpcode() != Mips::PHI) {
        RootLoPHI = RootHiPHI = nullptr;
      }
    } break;
    // detect candidates PHI having any ACC operands
    case acc::ExtractHi: {
      if (MachineInstr *MI2 = findExtractPair(MI)) {
        RootLoPHI = getPHIUsage(*MI2);
        RootHiPHI = getPHIUsage(MI);
        ExtractRoot = true;
      }
    } break;
    }
    if (!RootLoPHI || !RootHiPHI)
      continue;
    VisitedPHIs.insert(RootLoPHI);
    VisitedPHIs.insert(RootHiPHI);
    // go another time to find more usages
    if (ExtractRoot)
      Next = I;
    if (MachineInstr *NewPHI = ChangedPHIMap[RootLoPHI]) {
      // if RegClass is not acc::RC, it means that MI is an extract
      // so its usages should not be replaced
      if (MRI->getRegClass(MI.getOperand(0).getReg()) == acc::RC) {
        replaceRegUsesWith(MI.getOperand(0).getReg(),
                           NewPHI->getOperand(0).getReg());
        Modified = true;
      }
    } else {
      SmallVector<MachineInstr *, 4> LoPHIs;
      SmallVector<MachineInstr *, 4> HiPHIs;
      if (!collectFeedingAccPHIs(RootLoPHI, LoPHIs))
        continue;
      if (!collectFeedingAccPHIs(RootHiPHI, HiPHIs))
        continue;
      uint64_t CostBaseline = 0;
      uint64_t CostChange = 0;
      for (auto *PHI : llvm::reverse(LoPHIs))
        accConversionBeneficial(PHI, LoPHIs, partLow, CostBaseline, CostChange);
      for (auto *PHI : llvm::reverse(HiPHIs))
        accConversionBeneficial(PHI, HiPHIs, partHigh, CostBaseline,
                                CostChange);
      if (CostBaseline <= CostChange)
        continue;
      SmallVector<MIPair, 4> ZippedPHIs;
      if (compareAndZipPHIVec(LoPHIs, HiPHIs, ZippedPHIs)) {
        convertFeedingAccPHIs(ZippedPHIs);
        ConvertedPHIs.insert(LoPHIs.begin(), LoPHIs.end());
        ConvertedPHIs.insert(HiPHIs.begin(), HiPHIs.end());
        Modified = true;
      }
    }
  }
  return Modified;
}

// remove redundant copies in straight line code
bool NMDspPeephole::removeRedundantMoveFromAcc(MachineBasicBlock &MBB) {
  bool Modified = false;
  for (MachineBasicBlock::reverse_iterator I = MBB.rbegin(), E = MBB.rend();
       I != E; ++I) {
    MachineInstr &MI = *I;
    Register Acc = firstAccOperand(MI);
    if (Acc != Mips::NoRegister) {
      if (auto *MTLOHI = getMIDefiningReg(Acc, acc::Compose)) {
        auto OpLo = MTLOHI->getOperand(1).getReg();
        auto OpHi = MTLOHI->getOperand(2).getReg();
        MachineInstr *DefLo, *DefHi;
        if ((DefLo = getMIDefiningReg(OpLo, acc::ExtractLo)) &&
            (DefHi = getMIDefiningReg(OpHi, acc::ExtractHi))) {
          if (DefLo->getOperand(1).getReg() == DefHi->getOperand(1).getReg()) {
            MI.substituteRegister(Acc, DefHi->getOperand(1).getReg(), 0, *TRI);
            Modified = true;
          }
        }
      }
    }
  }
  return Modified;
}

// based on MachineRegisterInfo::replaceRegWith
void NMDspPeephole::replaceRegUsesWith(const Register FromReg, Register ToReg) {
  assert(FromReg != ToReg && "Cannot replace a reg with itself");

  // TODO: This could be more efficient by bulk changing the operands .
  for (MachineOperand &O :
       llvm::make_early_inc_range(MRI->reg_operands(FromReg))) {
    if (O.isDef())
      continue;
    if (ToReg.isPhysical()) {
      O.substPhysReg(ToReg, *TRI);
    } else {
      O.setReg(ToReg);
    }
  }
}

bool NMDspPeephole::runOnMachineFunction(MachineFunction &Fn) {
  if (DisableDspPeeps)
    return false;
  STI = &static_cast<const MipsSubtarget &>(Fn.getSubtarget());
  TRI = STI->getRegisterInfo();
  MRI = &Fn.getRegInfo();
  bool Modified = false;
  MachineBlockFrequencyInfo &MBFI = getAnalysis<MachineBlockFrequencyInfoWrapperPass>().getMBFI();
  this->MBFI = &MBFI;
  for (MachineFunction::iterator MFI = Fn.begin(), E = Fn.end(); MFI != E;
       ++MFI) {
    MachineBasicBlock &MBB = *MFI;
    Modified |= convertPHIsGPRToAcc(MBB);
    Modified |= removeRedundantMoveFromAcc(MBB);
  }
  return Modified;
}

namespace llvm {
FunctionPass *createNanoMipsDspPeepholePass() { return new NMDspPeephole(); }
} // namespace llvm
