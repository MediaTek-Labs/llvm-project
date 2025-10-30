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
static cl::opt<bool> DisableDspSinkExtract(
    "disable-nm-dsp-sink-extract",
    cl::desc("Disable Nanomips DSP peephole: sink ACC extract"),
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
  uint64_t BBFrequency(const MachineBasicBlock *);
  bool deadPHI(const MachineInstr &);
  void cloneForUse(MachineInstr &, MachineInstr *,
                   MachineFunction *,
                   SmallVector<MachineInstr *, 1> &);
  bool sinkExtract(MachineInstr &);
  bool sinkExtracts(MachineBasicBlock &);
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
  bool convertMtlohiToMult(Register, Register, Register,  MachineInstr &);
  bool convertAdd64ToAcc(Register OpLo, Register OpHi,
                             Register Acc,  MachineInstr &MI);
  bool removeRedundantAccMove(Register OpLo, Register OpHi,
                              Register Acc,  MachineInstr &MI);
  bool ApplyDspPeepholes(MachineBasicBlock &);
  MachineInstr *getPHIUsage(MachineInstr &);
  MachineInstr *getMIDefiningReg(Register, unsigned);
  MachineInstr *getMIDefiningOp(MachineInstr &, unsigned, unsigned);
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
 

uint64_t NMDspPeephole::BBFrequency(const MachineBasicBlock *BB) {
  BlockFrequency Freq = MBFI->getBlockFreq(BB);
  return Freq.getFrequency();
}

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

MachineInstr *NMDspPeephole::getMIDefiningOp(MachineInstr &MI, unsigned Op,
                                             unsigned Opcode) {
  assert(Op < MI.getNumOperands() && "Operand index out of range");
  return getMIDefiningReg(MI.getOperand(Op).getReg(), Opcode);
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

bool NMDspPeephole::deadPHI(const MachineInstr &MI) {
  // was not converted
  if (find(ConvertedPHIs, &MI) == ConvertedPHIs.end())
    return false;
  // was converted to ACC PHI, now look for non-compose usages
  for (const MachineOperand &MO : MI.defs()) {
    if (!MO.isReg() || !Register::isVirtualRegister(MO.getReg()))
      continue;

    for (auto &Use : MRI->use_nodbg_operands(MO.getReg())) {
      const MachineInstr *UseMI = Use.getParent();
      if (UseMI->getOpcode() != acc::Compose) {
        return false;
      }
    }
  }
  // could not find non-ACC compose usages
  return true;
}

void NMDspPeephole::cloneForUse(MachineInstr &MI, MachineInstr *U,
                                MachineFunction *MF,
                                SmallVector<MachineInstr *, 1> &DbgU) {
  MachineInstr *CloneMI = MF->CloneMachineInstr(&MI);
  unsigned OldReg = MI.getOperand(0).getReg();
  unsigned NewReg = MRI->createVirtualRegister(MRI->getRegClass(OldReg));

  auto ReplaceRegsAndInsert = [U, OldReg, NewReg](MachineInstr *MI) {
    for (MachineOperand &MO : MI->operands())
      if (MO.isReg() && MO.getReg() == OldReg)
        MO.setReg(NewReg);
    U->getParent()->insert(MachineBasicBlock::iterator(U), MI);
  };

  ReplaceRegsAndInsert(CloneMI);
  for (MachineOperand &MO : U->operands())
    if (MO.isReg() && MO.getReg() == OldReg)
      MO.setReg(NewReg);
  for (auto *UI : DbgU)
    ReplaceRegsAndInsert(MF->CloneMachineInstr(UI));
}

bool NMDspPeephole::sinkExtract(MachineInstr &MI) {
  MachineBasicBlock *BB = MI.getParent();
  uint64_t CostBaseline = BBFrequency(BB);
  uint64_t CostChange = 0;
  unsigned Reg = MI.getOperand(0).getReg();
  SmallVector<MachineInstr *, 4> U;
  SmallVector<MachineInstr *, 1> DbgU;
  for (MachineInstr &UseMI : MRI->use_instructions(Reg)) {
    if (UseMI.getOpcode() == Mips::PHI) {
      if (!deadPHI(UseMI))
        return false;
    } else if (UseMI.getParent() != BB) {
      // simple usage of the extracted value in a different block
      // TODO: extract just once per block
      CostChange += BBFrequency(UseMI.getParent());
      U.push_back(&UseMI);
    } else if (UseMI.isDebugValue()) {
      DbgU.push_back(&UseMI);
    } else
      // other usages within the same block
      return false;
  }
  MachineFunction *MF = BB->getParent();
  if (CostChange < CostBaseline && !U.empty()
      && (!MF->getFunction().hasOptSize() || U.size() < 2)) {
    for (auto *UI : U)
      cloneForUse(MI, UI, MF, DbgU);
    MI.eraseFromParent();
    for (auto *UI : DbgU)
      UI->eraseFromParent();
    return true;
  }
  return false;
}

bool NMDspPeephole::sinkExtracts(MachineBasicBlock &MBB) {
  bool Modified = false;
  for (MachineBasicBlock::reverse_iterator I = MBB.rbegin(), E = MBB.rend();
       I != E;) {
    MachineInstr &MI = *I;
    ++I;
    switch (MI.getOpcode()) {
    case acc::ExtractHi:
    case acc::ExtractLo:
      Modified |= sinkExtract(MI);
    }
  }
  return Modified;
}

// replace two final instructions with  a single one:
//  acc = PseudoMTLOHI_DSP_NM(zero, zero)
// ==>
//  acc = MULT_DSP_NM(zero, zero)

bool NMDspPeephole::convertMtlohiToMult(Register OpLo, Register OpHi,
                                      Register Acc,  MachineInstr &MI) {
  if (OpLo == OpHi) {
    MachineInstr *OpLoInstr = getMIDefiningReg(OpLo, Mips::COPY);
    if (OpLoInstr && OpLoInstr->getOperand(1).getReg() == Mips::ZERO_NM) {
      const MipsInstrInfo *TII = STI->getInstrInfo();
      MI.setDesc(TII->get(Mips::MULT_DSP_NM));
      LLVM_DEBUG(dbgs() << "Converting MTLOHI into:\n");
      LLVM_DEBUG(MI.dump());
      return true;
    }
  }
  return false;
}
//   ac1 = {addwc(hi(ac0), 0),
//          addsc(lo(ac0), x)}
//  ==>
//   ac1 = madd(ac0, x, 1)
bool NMDspPeephole::convertAdd64ToAcc(Register OpLo, Register OpHi,
                                      Register Acc,  MachineInstr &MI) {

  // check no other uses since we erase manually
  if (!MRI->hasAtMostUserInstrs(OpLo, 2) ||
      !MRI->hasAtMostUserInstrs(OpHi, 2))
    return false;

  // both parts are defined in a 64-bit addition in PGRs
  MachineInstr *AddLo, *AddHi;
  if ((AddLo = getMIDefiningReg(OpLo, Mips::ADDSC_NM)) &&
      (AddHi = getMIDefiningReg(OpHi, Mips::ADDWC_NM))) {

    Register AddLoReg1 = AddLo->getOperand(1).getReg();
    Register AddHiReg1 = AddHi->getOperand(1).getReg();

    // check no other uses since we erase manually
    if (!MRI->hasAtMostUserInstrs(AddLoReg1, 2) ||
        !MRI->hasAtMostUserInstrs(AddHiReg1, 2))
      return false;

    // first operand of both parts of 64-bit addition
    // was extracted from a DSP accumulator
    MachineInstr *AccLo, *AccHi;
    if ((AccLo = getMIDefiningReg(AddLoReg1, acc::ExtractLo)) &&
        (AccHi = getMIDefiningReg(AddHiReg1, acc::ExtractHi))) {

       // AccLo and AccHi were extracted from the same accumulator
      if (AccLo->getOperand(1).getReg() != AccHi->getOperand(1).getReg())
        return false;

      // Accumulator's high part has not been changed
      MachineInstr *AddHiOp2 = getMIDefiningOp(*AddHi, 2, Mips::COPY);
      if (!AddHiOp2 || AddHiOp2->getOperand(1).getReg() != Mips::ZERO_NM)
        return false;

      auto *MBB = MI.getParent();
      const MipsInstrInfo *TII = STI->getInstrInfo();

      // try to reuse "1"
      Register One, LowPartReg = AddLo->getOperand(2).getReg();
      MachineInstr *AddLoOp2 = getMIDefiningOp(*AddLo, 2, Mips::PseudoLI_NM);
      if (AddLoOp2 && AddLoOp2->getOperand(1).getImm() == 1LL) {
        //acc + 1  ==> madd acc 1, 1
        One = LowPartReg;
      } else {
         //acc + x  ==> madd acc x, 1
         One = MRI->createVirtualRegister(&Mips::GPRNM32RegClass);
         BuildMI(*MBB, AddLo->getIterator(),
                 AddLo->getDebugLoc(), TII->get(Mips::PseudoLI_NM), One)
           .addImm(1);
      }
      Register AccNew = MRI->createVirtualRegister(acc::RC);
      MachineInstr *MADD = BuildMI(*MBB, MI.getIterator(),
                                   MI.getDebugLoc(),
                                   TII->get(Mips::MADD_DSP_NM), AccNew)
                             .addReg(LowPartReg)
                             .addReg(One)
                             .add(AccLo->getOperand(1));

      LLVM_DEBUG(dbgs() << "Converting 64-bit add:\n");
      LLVM_DEBUG(AddLo->dump());
      LLVM_DEBUG(AddHi->dump());
      LLVM_DEBUG(dbgs() << "To:\n");
      LLVM_DEBUG(MADD->dump());

      replaceRegUsesWith(Acc, AccNew);

      // side effects we don't care about, clean manually
      AddLo->eraseFromParent(); // side effect: carry bit
      AddHi->eraseFromParent(); // side effect: overflow
    }
  }
  return false;
}

// remove redundant copies in straight line code
bool NMDspPeephole::removeRedundantAccMove(Register OpLo, Register OpHi,
                                           Register Acc,  MachineInstr &MI) {
  MachineInstr *DefLo, *DefHi;
  if ((DefLo = getMIDefiningReg(OpLo, acc::ExtractLo)) &&
      (DefHi = getMIDefiningReg(OpHi, acc::ExtractHi))) {
    if (DefLo->getOperand(1).getReg() == DefHi->getOperand(1).getReg()) {

      LLVM_DEBUG(dbgs() << "Eliminating:\n");
      LLVM_DEBUG(DefLo->dump());
      LLVM_DEBUG(DefHi->dump());
      LLVM_DEBUG(dbgs() << "By using '"
                 << DefHi->getOperand(1)
                 << "' directly in:\n");
      LLVM_DEBUG(MI.dump());

      MI.substituteRegister(Acc, DefHi->getOperand(1).getReg(), 0, *TRI);
      return true;
    }
  }
  return false;
}

bool NMDspPeephole::ApplyDspPeepholes(MachineBasicBlock &MBB) {
  bool Modified = false;
  for (MachineBasicBlock::reverse_iterator I = MBB.rbegin(), E = MBB.rend();
       I != E; ++I) {
    MachineInstr &MI = *I;
    Register Acc = firstAccOperand(MI);
    if (Acc != Mips::NoRegister) {
      if (auto *MTLOHI = getMIDefiningReg(Acc, acc::Compose)) {
        auto OpLo = MTLOHI->getOperand(1).getReg();
        auto OpHi = MTLOHI->getOperand(2).getReg();
        if (removeRedundantAccMove(OpLo, OpHi, Acc, MI) ||
            convertAdd64ToAcc(OpLo, OpHi, Acc, *MTLOHI) ||
            convertMtlohiToMult(OpLo, OpHi, Acc, *MTLOHI)) {
          Modified = true;
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
    Modified |= ApplyDspPeepholes(MBB);
    if (!DisableDspSinkExtract)
      Modified |= sinkExtracts(MBB);
  }
  return Modified;
}

namespace llvm {
FunctionPass *createNanoMipsDspPeepholePass() { return new NMDspPeephole(); }
} // namespace llvm
