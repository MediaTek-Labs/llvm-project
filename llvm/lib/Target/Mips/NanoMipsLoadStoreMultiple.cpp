//===- NanoMipsLoadStoreMultiple.cpp - nanoMIPS load / store opt. pass
//--------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file This file contains a pass that performs load / store related peephole
/// optimizations. This pass should be run after register allocation.
//
//===----------------------------------------------------------------------===//

#include "Mips.h"
#include "MipsSubtarget.h"
#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/InitializePasses.h"

#include <cmath>

using namespace llvm;

#define DEBUG_TYPE "nanomips-lwm-swm"
#define NM_LOAD_STORE_OPT_NAME "nanoMIPS load/store multiple optimization pass"

static cl::opt<bool> DisableNMLoadStoreMultiple(
    "disable-nm-lwm-swm", cl::Hidden, cl::init(false),
    cl::desc("Disable NanoMips load/store multiple optimizations"));

namespace {
struct NMLoadStoreMultipleOpt : public MachineFunctionPass {
  struct LSIns {
    unsigned Rt;
    unsigned Rs;
    int64_t Offset;
    MachineBasicBlock *MBB;
    MachineInstr *MI;

    LSIns(MachineInstr *MI) {
      this->MI = MI;
      MBB = MI->getParent();
      Rt = MI->getOperand(0).getReg().id();
      Rs = MI->getOperand(1).getReg().id();
      Offset = MI->getOperand(2).getImm();
    }
  };
  using InstrList = SmallVector<MachineInstr *, 4>;
  using MBBIter = MachineBasicBlock::iterator;
  struct Candidate {
    InstrList Sequence;
    size_t GapSize;
    bool Move = false;
  };
  using CandidateList = SmallVector<Candidate, 3>;
  static char ID;
  const MipsSubtarget *STI;
  const TargetInstrInfo *TII;
  const TargetRegisterInfo *TRI;
  const MachineRegisterInfo *MRI;
  MCRegisterClass RC = MipsMCRegisterClasses[Mips::GPRNM32RegClassID];
  DenseMap<unsigned, unsigned> RegToIndexMap;

  NMLoadStoreMultipleOpt() : MachineFunctionPass(ID) {
    // Initialize RegToIndexMap.
    for (unsigned I = 0; I < RC.getNumRegs(); I++) {
      unsigned R = RC.begin()[I];
      RegToIndexMap[R] = I;
    }
  }
  StringRef getPassName() const override { return NM_LOAD_STORE_OPT_NAME; }
  bool runOnMachineFunction(MachineFunction &Fn) override;
  unsigned getRegNo(unsigned Reg);
  bool isValidLoadStore(MachineInstr &MI, bool IsLoad, InstrList);
  bool isValidNextLoadStore(LSIns Prev, LSIns Next, size_t &GapSize,
                            size_t &CurrSeqSize, bool &RegGap);
  bool generateLoadStoreMultiple(MachineBasicBlock &MBB, bool IsLoad);
  void sortLoadStoreList(InstrList &LoadStoreList, bool IsLoad);
  void findCandidatesForOptimization(InstrList &LoadStoreList,
                                     CandidateList &Candidates);
};
} // namespace

char NMLoadStoreMultipleOpt::ID = 0;

bool NMLoadStoreMultipleOpt::runOnMachineFunction(MachineFunction &Fn) {
  if (DisableNMLoadStoreMultiple)
    return false;
  STI = &static_cast<const MipsSubtarget &>(Fn.getSubtarget());
  TII = STI->getInstrInfo();
  TRI = STI->getRegisterInfo();
  MRI = &Fn.getRegInfo();
  bool Modified = false;
  for (MachineFunction::iterator MFI = Fn.begin(), E = Fn.end(); MFI != E;
       ++MFI) {
    MachineBasicBlock &MBB = *MFI;
    Modified |= generateLoadStoreMultiple(MBB, /*IsLoad=*/false);
    Modified |= generateLoadStoreMultiple(MBB, /*IsLoad=*/true);
  }

  return Modified;
}

unsigned NMLoadStoreMultipleOpt::getRegNo(unsigned Reg) {
  auto I = RegToIndexMap.find(Reg);

  // Invalid register index.
  if (I == RegToIndexMap.end())
    return RC.getNumRegs();

  return I->second;
}

// Here, we're sorting InstrList to be able to easily recognize sequences that
// are not sorted by the reg-offset pair. We're sorting ascending by register
// number. Later we check if the offsets are in the desired order. The
// exceptions are zero register stores. In that case, the sorting is done by the
// offset.
// Currently, the following case is not supported:
// lw a30, 4 (a9)
// lw a31, 8 (a9)
// lw a16, 12(a9)
void NMLoadStoreMultipleOpt::sortLoadStoreList(InstrList &LoadStoreList,
                                               bool IsLoad) {
  auto CompareInstructions = [this, IsLoad](MachineInstr *First,
                                            MachineInstr *Second) {
    Register FirstReg = First->getOperand(0).getReg();
    Register SecondReg = Second->getOperand(0).getReg();
    unsigned FirstRegNo = getRegNo(FirstReg);
    unsigned SecondRegNo = getRegNo(SecondReg);

    // For the zero register stores, sort instructions by the Offset.
    if (!IsLoad && FirstRegNo == 0 && SecondRegNo == 0)
      return First->getOperand(2).getImm() < Second->getOperand(2).getImm();
    return FirstRegNo < SecondRegNo;
  };
  std::sort(LoadStoreList.begin(), LoadStoreList.end(), CompareInstructions);
}

void NMLoadStoreMultipleOpt::findCandidatesForOptimization(
    InstrList &LoadStoreList, CandidateList &Candidates) {
  InstrList Sequence;
  size_t GapSize = 0, SeqSize = 0;
  bool RegGap = false;

  auto clearSeqence = [&Sequence, &GapSize, &SeqSize, &RegGap]() {
    Sequence.clear();
    GapSize = 0;
    SeqSize = 0;
    RegGap = false;
  };

  for (auto &MI : LoadStoreList) {
    // Sequences cannot be longer than 8 instructions.
    if (SeqSize == 8) {
      Candidates.push_back({Sequence, GapSize});
      clearSeqence();
    }
    // When starting a new sequence, there's no need to do any checks.
    if (Sequence.empty()) {
      Sequence.push_back(MI);
      SeqSize = 1;
      continue;
    }

    if (!isValidNextLoadStore(Sequence.back(), MI, GapSize, SeqSize, RegGap)) {
      if (SeqSize > 1)
        Candidates.push_back({Sequence, GapSize});
      clearSeqence();
    }

    Sequence.push_back(MI);
    SeqSize++;

    if (RegGap) {
      Candidates.push_back({Sequence, GapSize, true});
      clearSeqence();
    }
  }

  // Save the last valid sequence for this list. At least 2 instructions are
  // neccessary for a valid sequence.
  if (SeqSize > 1)
    Candidates.push_back({Sequence, GapSize});
}

// All instruction in the seqence should have the same Rs register, and
// different Rt register.
bool NMLoadStoreMultipleOpt::isValidLoadStore(MachineInstr &MI, bool IsLoad,
                                              InstrList Sequence) {
  unsigned Opcode = MI.getOpcode();
  // Make sure the instruction doesn't have any atomic, volatile or
  // otherwise strictly ordered accesses.
  for (auto &MMO : MI.memoperands())
    if (MMO->isAtomic() || !MMO->isUnordered())
      return false;

  Register Rt, Rs;
  if (IsLoad) {
    // TODO: Handle unaligned loads and stores.
    if (Opcode != Mips::LW_NM && Opcode != Mips::LWs9_NM)
      return false;

    Rt = MI.getOperand(0).getReg();
    Rs = MI.getOperand(1).getReg();

    // TODO: Rt and Rs can be equal, but only if that is the last load of
    // the sequence.
    if (Rt == Rs)
      return false;

  } else {
    if (Opcode != Mips::SW_NM && Opcode != Mips::SWs9_NM)
      return false;
    Rt = MI.getOperand(0).getReg();
    Rs = MI.getOperand(1).getReg();
  }

  if (Sequence.size() > 0) {
    auto SeqRs = Sequence.back()->getOperand(1).getReg();
    if (Rs != SeqRs)
      return false;
  }
  auto RtExists = [&Rt](const MachineInstr *I) {
    return I->getOperand(0).getReg() == Rt;
  };
  auto It = std::find_if(Sequence.begin(), Sequence.end(), RtExists);
  // Zero register stores are a special case that does not require consequent
  // $rt registers, but instead requires all $rt registers to be $zero.
  if (It == Sequence.end() || getRegNo(Rt) == 0)
    return true;
  return false;
}

bool NMLoadStoreMultipleOpt::isValidNextLoadStore(LSIns Prev, LSIns Next,
                                                  size_t &GapSize,
                                                  size_t &CurrSeqSize,
                                                  bool &RegGap) {
  unsigned PrevRtNo = getRegNo(Prev.Rt);
  unsigned DesiredRtNo = PrevRtNo != 0 ? (PrevRtNo + 1) : 0;
  Register DesiredRtReg = RC.getRegister(DesiredRtNo);
  if (Next.Offset == Prev.Offset + 4) {
    if (Next.Rt == DesiredRtReg)
      return true;
    // Next.Rt != DesiredRtReg
    // GAP, but offset ok
    // lw a0, 8(a4)
    // lw a1, 12(a4)
    // lw a3, 16(a4)
    // For now, the instruction like lw a3, 16(a4) interrupts the sequence.
    if (CurrSeqSize < 2)
      return false;

    assert(Register::isPhysicalRegister(DesiredRtNo) &&
           "Desired register is not physical!");
    if (MachineBasicBlock::LQR_Dead !=
        Prev.MBB->computeRegisterLiveness(TRI, DesiredRtReg, Prev.MI))
      return false;

    RegGap = true;
    return true;
  }
  // Next.Offset != Prev.Offset + 4
  bool OffsetOk = ((Next.Offset - Prev.Offset) % 4) == 0;
  unsigned Gap = abs((Next.Offset - Prev.Offset) / 4 - 1);
  if (OffsetOk && (CurrSeqSize + Gap + 1 <= 8) &&
      Next.Rt == RC.getRegister(PrevRtNo + Gap + 1)) {
    // "full" GAP
    // lw a0, 8(a4)
    // lw a1, 12(a4)
    // lw a3, 20(a4)
    for (size_t i = 0; i < Gap; i++) {
      assert(Register::isPhysicalRegister(DesiredRtNo + i) &&
             "Desired register is not physical!");
      if (MachineBasicBlock::LQR_Dead !=
          Prev.MBB->computeRegisterLiveness(TRI, DesiredRtReg, Prev.MI))
        return false;
      DesiredRtReg = RC.getRegister(DesiredRtNo + i + 1);
    }
    GapSize += Gap;
    CurrSeqSize += Gap;
    return true;
  }
  return false;
}

bool NMLoadStoreMultipleOpt::generateLoadStoreMultiple(MachineBasicBlock &MBB,
                                                       bool IsLoad) {
  bool Modified = false;

  // TODO: Consider allowing interspersed arithmetic/logical operations in
  // load/store sequences to reduce sensitivity to instruction ordering. Note
  // that proper scheduling models will alter instruction order, increasing
  // mixed memory and compute operations. Dependency checks will be required.
  InstrList SequenceToSort;
  SmallVector<InstrList, 3> SequenceList;
  for (auto &MI : MBB) {
    // CFI and debug instructions don't break the sequence.
    if (MI.isCFIInstruction() || MI.isDebugInstr())
      continue;
    if (isValidLoadStore(MI, IsLoad, SequenceToSort)) {
      SequenceToSort.push_back(&MI);
      continue;
    }
    if (SequenceToSort.size() > 1) {
      SequenceList.push_back(SequenceToSort);
      SequenceToSort.clear();
    }
  }

  CandidateList Candidates;
  InstrList Sequence;
  for (size_t i = 0; i < SequenceList.size(); i++) {
    sortLoadStoreList(SequenceList[i], IsLoad);
    findCandidatesForOptimization(SequenceList[i], Candidates);
  }

  for (auto &C : Candidates) {
    auto Seq = C.Sequence;
    assert(Seq.size() > 1 && Seq.size() < 9);
    auto *Base = Seq.front();
    int64_t Offset = Base->getOperand(2).getImm();
    // Sequence cannot be merged, if the offset is out of range.
    if (!isInt<9>(Offset))
      continue;

    auto InsertBefore = std::next(MBBIter(Base));
    unsigned Opcode = IsLoad ? Mips::LWM_NM : Mips::SWM_NM;
    auto BMI =
        BuildMI(MBB, InsertBefore, Base->getDebugLoc(), TII->get(Opcode))
            .addReg(Base->getOperand(0).getReg(), IsLoad ? RegState::Define : 0)
            .addReg(Base->getOperand(1).getReg())
            .addImm(Offset)
            .addImm(Seq.size() + C.GapSize);
    BMI.cloneMergedMemRefs(Seq);
    if (C.Move) {
      BuildMI(MBB, std::next(MBBIter(BMI.getInstr())), Base->getDebugLoc(),
              TII->get(Mips::MOVE_NM))
          .addReg(Seq.back()->getOperand(0).getReg(), RegState::Define)
          .addReg(Seq[Seq.size() - 2]->getOperand(0).getReg() + 1);
    }
    for (auto *MI : Seq) {
      if (MI != Base)
        BMI.addReg(MI->getOperand(0).getReg(),
                   IsLoad ? RegState::ImplicitDefine : RegState::Implicit);
      MBB.erase(MI);
    }

    Modified = true;
  }
  return Modified;
}

INITIALIZE_PASS(NMLoadStoreMultipleOpt, DEBUG_TYPE, NM_LOAD_STORE_OPT_NAME,
                false, false)

namespace llvm {
FunctionPass *createNanoMipsLoadStoreMultiplePass() {
  return new NMLoadStoreMultipleOpt();
}
} // namespace llvm