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

namespace {
struct NMLoadStoreMultipleOpt : public MachineFunctionPass {
  struct LSIns {
    unsigned Rt;
    unsigned Rs;
    int64_t Offset;
    MachineBasicBlock *MBB;

    LSIns(MachineInstr *MI) {
      MBB = MI->getParent();
      Rt = MI->getOperand(0).getReg().id();
      Rs = MI->getOperand(1).getReg().id();
      Offset = MI->getOperand(2).getImm();
    }
  };
  using InstrList = SmallVector<MachineInstr *, 4>;
  using MBBIter = MachineBasicBlock::iterator;
  static char ID;
  const MipsSubtarget *STI;
  const TargetInstrInfo *TII;
  const TargetRegisterInfo *TRI;
  const MachineRegisterInfo *MRI;

  const std::unordered_map<unsigned, unsigned> CalleeSaves{
      {Mips::GP_NM, 0}, {Mips::FP_NM, 1}, {Mips::RA_NM, 2},  {Mips::S0_NM, 3},
      {Mips::S1_NM, 4}, {Mips::S2_NM, 5}, {Mips::S3_NM, 6},  {Mips::S4_NM, 7},
      {Mips::S5_NM, 8}, {Mips::S6_NM, 9}, {Mips::S7_NM, 10},
  };
  MCRegisterClass RC = MipsMCRegisterClasses[Mips::GPR32RegClassID];

  NMLoadStoreMultipleOpt() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override { return NM_LOAD_STORE_OPT_NAME; }
  bool runOnMachineFunction(MachineFunction &Fn) override;
  unsigned getRegNo(unsigned Reg);
  bool isValidLoadStore(MachineInstr &MI, bool IsLoad, InstrList);
  bool isValidNextLoadStore(LSIns Prev, LSIns Next, size_t &GapSize,
                            size_t &CurrSeqSize, bool &RegGap);
  bool generateLoadStoreMultiple(MachineBasicBlock &MBB, bool IsLoad);
  void sortLoadStoreList(InstrList &LoadStoreList, bool IsLoad);
};
} // namespace

char NMLoadStoreMultipleOpt::ID = 0;

bool NMLoadStoreMultipleOpt::runOnMachineFunction(MachineFunction &Fn) {
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
  for (unsigned I = 0; I < RC.getNumRegs(); I++) {
    unsigned R = RC.begin()[I];
    if (R == Reg)
      return I;
  }
  // Invalid register index.
  return RC.getNumRegs();
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

// All instruction in the seqence should have the same Rs register, and
// different Rt register.
bool NMLoadStoreMultipleOpt::isValidLoadStore(MachineInstr &MI, bool IsLoad,
                                              InstrList Sequence) {
  unsigned Opcode = MI.getOpcode();
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
    // GAP, but offset ok
    // lw a0, 8(a4)
    // lw a1, 12(a4)
    // lw a3, 16(a4)
    if (Next.Rt != DesiredRtReg) {
      // For now, the instruction like lw a3, 16(a4) insterupts the sequence.
      if (CurrSeqSize < 2)
        return false;

      LivePhysRegs LiveRegs(*TRI);
      computeLiveIns(LiveRegs, *Prev.MBB);
      assert(Register::isPhysicalRegister(DesiredRtNo) &&
             "Desired register is not physical!");
      if (!LiveRegs.available(*MRI, (DesiredRtReg)))
        return false;

      RegGap = true;
      return true;
    } else {
      return true;
    }
  } else {
    // "full" GAP
    // lw a0, 8(a4)
    // lw a1, 12(a4)
    // lw a3, 20(a4)
    bool OffsetOk = ((Next.Offset - Prev.Offset) % 4) == 0;
    int Gap = (Next.Offset - Prev.Offset) / 4 - 1;
    if (OffsetOk && (CurrSeqSize + Gap + 1 <= 8) &&
        Next.Rt == RC.getRegister(PrevRtNo + Gap + 1)) {
      LivePhysRegs LiveRegs(*TRI);
      computeLiveIns(LiveRegs, *Prev.MBB);
      for (size_t i = 0; i < Gap; i++) {
        assert(Register::isPhysicalRegister(DesiredRtNo + i) &&
               "Desired register is not physical!");
        if (!LiveRegs.available(*MRI, (DesiredRtReg)))
          return false;
        DesiredRtReg = RC.getRegister(DesiredRtNo + i + 1);
      }
      GapSize += Gap;
      CurrSeqSize += Gap;
      return true;
    }
  }
  return false;
}

bool NMLoadStoreMultipleOpt::generateLoadStoreMultiple(MachineBasicBlock &MBB,
                                                       bool IsLoad) {
  bool Modified = false;
  struct Candidate {
    InstrList Sequence;
    size_t GapSize;
    bool Move = false;
  };
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

  SmallVector<Candidate, 3> Candidates;
  InstrList Sequence;
  size_t GapSize = 0;
  size_t SeqSize = 0;
  bool RegGap = false;
  for (size_t i = 0; i < SequenceList.size(); i++) {
    sortLoadStoreList(SequenceList[i], IsLoad);
    for (auto &MI : SequenceList[i]) {
      // Sequences cannot be longer than 8 instructions.
      if (SeqSize == 8) {
        Candidates.push_back({Sequence, GapSize});
        Sequence.clear();
        GapSize = 0;
        SeqSize = 0;
        RegGap = false;
      }
      // When starting a new sequence, there's no need to do any checks.
      if (Sequence.empty()) {
        Sequence.push_back(MI);
        SeqSize = 1;
        continue;
      }

      if (!isValidNextLoadStore(Sequence.back(), MI, GapSize, SeqSize,
                                RegGap)) {
        if (SeqSize > 1)
          Candidates.push_back({Sequence, GapSize});
        Sequence.clear();
        GapSize = 0;
        SeqSize = 0;
        RegGap = false;
      }

      Sequence.push_back(MI);
      SeqSize++;

      if (RegGap) {
        Candidates.push_back({Sequence, GapSize, true});
        Sequence.clear();
        GapSize = 0;
        SeqSize = 0;
        RegGap = false;
      }
      continue;
    }

    // At least 2 instructions are neccessary for a valid sequence.
    if (SeqSize > 1) {
      Candidates.push_back({Sequence, GapSize});
      SeqSize++;
    }

    // Sequence has either ended or has never been started.
    if (!Sequence.empty()) {
      Sequence.clear();
      SeqSize = 0;
      GapSize = 0;
      RegGap = false;
    }
  }

  // Make sure that the last sequence has been added to the Candidates list.
  // TODO: Check if needed.
  if (SeqSize > 1) {
    Candidates.push_back({Sequence, GapSize});
    SeqSize++;
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