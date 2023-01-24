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
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
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

    LSIns(MachineInstr *MI) {
      Rt = MI->getOperand(0).getReg().id();
      Rs = MI->getOperand(1).getReg().id();
      Offset = MI->getOperand(2).getImm();
    }
  };
  using InstrList = SmallVector<MachineInstr *, 11>;
  using MBBIter = MachineBasicBlock::iterator;
  static char ID;
  const MipsSubtarget *STI;
  const TargetInstrInfo *TII;
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
  bool isValidLoadStore(MachineInstr &MI, bool IsLoad);
  bool isValidNextLoadStore(LSIns Prev, LSIns Next, bool &IsAscending);
  bool generateLoadStoreMultiple(MachineBasicBlock &MBB, bool IsLoad);
};
} // namespace

char NMLoadStoreMultipleOpt::ID = 0;

bool NMLoadStoreMultipleOpt::runOnMachineFunction(MachineFunction &Fn) {
  if (DisableNMLoadStoreMultiple)
    return false;
  STI = &static_cast<const MipsSubtarget &>(Fn.getSubtarget());
  TII = STI->getInstrInfo();
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

bool NMLoadStoreMultipleOpt::isValidLoadStore(MachineInstr &MI, bool IsLoad) {
  unsigned Opcode = MI.getOpcode();
  // Make sure the instruction doesn't have any atomic, volatile or
  // otherwise strictly ordered accesses.
  for (auto &MMO : MI.memoperands())
    if (MMO->isAtomic() || !MMO->isUnordered())
      return false;

  Register Rt, Rs;
  if (IsLoad) {
    // TODO: Handle unaligned loads and stores.
    if (Opcode == Mips::LW_NM || Opcode == Mips::LWs9_NM) {
      // TODO: Rt and Rs can be equal, but only if that is the last load of
      // the sequence.
      Register Rt = MI.getOperand(0).getReg();
      Register Rs = MI.getOperand(1).getReg();
      if (Rt != Rs)
        return true;
    }
  } else {
    if (Opcode == Mips::SW_NM || Opcode == Mips::SWs9_NM)
      return true;
  }
  return false;
}

bool NMLoadStoreMultipleOpt::isValidNextLoadStore(LSIns Prev, LSIns Next,
                                                  bool &IsAscending) {
  if (Prev.Rs != Next.Rs)
    return false;

  unsigned PrevRtNo = getRegNo(Prev.Rt);
  if (Next.Offset == Prev.Offset + 4) {
    // Zero register stores are a special case that does not require
    // consequent $rt registers, but instead requires all $rt
    // registers to be $zero.
    // After processing $31, sequence continues from $16.
    unsigned DesiredRtNo =
        PrevRtNo != 0 ? (PrevRtNo == 31 ? 16 : PrevRtNo + 1) : 0;
    if (Next.Rt != RC.getRegister(DesiredRtNo))
      return false;

    IsAscending = true;
    return true;
  } else if (Next.Offset == Prev.Offset - 4) {
    // In case the previous register was $16 and the sequence happens to
    // to go backwards, the next register can be either $15 or $31.
    if (PrevRtNo == 16) {
      if (Next.Rt != RC.getRegister(PrevRtNo - 1) &&
          Next.Rt != RC.getRegister(31))
        return false;
    } else {
      // Zero register stores are a special case that does not require
      // consequent $rt registers, but instead requires all $rt
      // registers to be $zero.
      unsigned DesiredRtNo = PrevRtNo != 0 ? PrevRtNo - 1 : 0;
      if (Next.Rt != RC.getRegister(DesiredRtNo))
        return false;
    }
    IsAscending = false;
    return true;
  }
  return false;
}

bool NMLoadStoreMultipleOpt::generateLoadStoreMultiple(MachineBasicBlock &MBB,
                                                       bool IsLoad) {
  struct Candidate {
    SmallVector<MachineInstr *> Sequence;
    bool IsAscending;
  };
  bool Modified = false;
  SmallVector<Candidate> Candidates;
  SmallVector<MachineInstr *> Sequence;
  bool IsAscending;
  for (auto &MI : MBB) {
    // CFI and debug instructions don't break the sequence.
    if (MI.isCFIInstruction() || MI.isDebugInstr())
      continue;

    if (isValidLoadStore(MI, IsLoad)) {
      // Sequences cannot be longer than 8 instructions.
      if (Sequence.size() == 8) {
        Candidates.push_back({Sequence, IsAscending});
        Sequence.clear();
      }
      // When starting a new sequence, there's no need to do any checks.
      if (Sequence.empty()) {
        Sequence.push_back(&MI);
        continue;
      }
      bool ShouldStartNewSequence = false;
      bool IsNextAscending;
      if (isValidNextLoadStore(Sequence.back(), &MI, IsNextAscending)) {
        if (Sequence.size() > 1) {
          // In case the next instruction is going in the opposite direction
          // from the sequence, start a new sequence.
          if (IsAscending != IsNextAscending) {
            ShouldStartNewSequence = true;
          }
        } else {
          IsAscending = IsNextAscending;
        }
      } else {
        // In case the next instruction is not a valid successor, save the
        // current sequence (if we have one) and create a new sequence.
        ShouldStartNewSequence = true;
      }

      if (ShouldStartNewSequence) {
        if (Sequence.size() > 1)
          Candidates.push_back({Sequence, IsAscending});
        Sequence.clear();
      }

      Sequence.push_back(&MI);
      continue;
    }

    // At least 2 instructions are neccessary for a valid sequence.
    if (Sequence.size() > 1)
      Candidates.push_back({Sequence, IsAscending});

    // Sequence has either ended or has never been started.
    if (!Sequence.empty())
      Sequence.clear();
  }

  // Make sure that the last sequence has been added to the Candidates list.
  if (Sequence.size() > 1)
    Candidates.push_back({Sequence, IsAscending});

  // Separate sequence to avoid removing instructions from MBB while iterating.
  for (auto &C : Candidates) {
    auto &Seq = C.Sequence;

    assert(Seq.size() > 1 && Seq.size() < 9);

    auto *Base = C.IsAscending ? Seq.front() : Seq.back();
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
            .addImm(Seq.size());
    BMI.cloneMergedMemRefs(Seq);
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