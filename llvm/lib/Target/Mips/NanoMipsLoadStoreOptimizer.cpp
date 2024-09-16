//===- ARMLoadStoreOptimizer.cpp - nanoMIPS load / store opt. pass --------===//
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
#include <cmath>

using namespace llvm;

#define PASS_NAME "nanoMIPS load/store optimization pass"
#define DEBUG_TYPE "nmloadstoreopt"

static cl::opt<bool>
DisableNMSaveRestore("disable-nm-save-restore", cl::Hidden, cl::init(false),
                     cl::desc("Disable NanoMips save/restore optimizations"));

static cl::opt<bool>
DisableNMPCRelOpt("disable-nm-pcrel-opt", cl::Hidden, cl::init(false),
                  cl::desc("Disable NanoMips PC-relative addressing optimization"));

namespace {
struct NMLoadStoreOpt : public MachineFunctionPass {
  using InstrList = SmallVector<MachineInstr *, 11>;
  using MBBIter = MachineBasicBlock::iterator;
  static char ID;
  const MipsSubtarget *STI;
  const TargetInstrInfo *TII;
  const std::unordered_map<unsigned, unsigned> CalleeSaves{
      {Mips::GP_NM, 0}, {Mips::FP_NM, 1}, {Mips::RA_NM, 2},  {Mips::S0_NM, 3},
      {Mips::S1_NM, 4}, {Mips::S2_NM, 5}, {Mips::S3_NM, 6},  {Mips::S4_NM, 7},
      {Mips::S5_NM, 8}, {Mips::S6_NM, 9}, {Mips::S7_NM, 10},
  };
  MCRegisterClass RC = MipsMCRegisterClasses[Mips::GPRNM32RegClassID];

  NMLoadStoreOpt() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override { return PASS_NAME; }
  bool runOnMachineFunction(MachineFunction &Fn) override;
  bool isReturn(MachineInstr &MI);
  bool isTailCall(MachineInstr &MI);
  bool isMustTailCall(MachineInstr &MI);
  bool isStackPointerAdjustment(MachineInstr &MI, bool IsRestore);
  bool isCalleeSavedLoadStore(MachineInstr &MI, bool IsRestore);
  void sortCalleeSavedLoadStoreList(InstrList &LoadStoreList);
  bool isExtensible(MachineInstr &MI, InstrList &LoadStoreList);
  bool isRASaved(const InstrList &StoreSequence);
  bool isValidSaveRestore16Offset(int64_t Offset);
  bool generateSaveOrRestore(MachineBasicBlock &MBB, bool IsRestore);
  bool generatePCRelative(MachineBasicBlock &MBB);
};
} // namespace

char NMLoadStoreOpt::ID = 0;

bool NMLoadStoreOpt::runOnMachineFunction(MachineFunction &Fn) {
  STI = &static_cast<const MipsSubtarget &>(Fn.getSubtarget());
  TII = STI->getInstrInfo();
  bool Modified = false;
  for (MachineFunction::iterator MFI = Fn.begin(), E = Fn.end(); MFI != E;
       ++MFI) {
    MachineBasicBlock &MBB = *MFI;
    if (!DisableNMSaveRestore) {
      Modified |= generateSaveOrRestore(MBB, /*IsRestore=*/false);
      Modified |= generateSaveOrRestore(MBB, /*IsRestore=*/true);
    }
    if (!DisableNMPCRelOpt)
      Modified |= generatePCRelative(MBB);
  }

  return Modified;
}

bool NMLoadStoreOpt::isStackPointerAdjustment(MachineInstr &MI,
                                              bool IsRestore) {
  if (MI.getOpcode() != Mips::ADDIU_NM &&
      MI.getOpcode() != Mips::ADDIUNEG_NM)
    return false;
  Register DstReg = MI.getOperand(0).getReg();
  Register SrcReg = MI.getOperand(1).getReg();
  if (DstReg != Mips::SP_NM || SrcReg != Mips::SP_NM)
    return false;
  int64_t Imm = MI.getOperand(2).getImm();
  if (IsRestore && Imm <= 0)
    return false;
  if (!IsRestore && Imm >= 0)
    return false;
  Imm = std::abs(Imm);
  // Adjustment has to be doubleword aligned.
  if (Imm & 0x7)
    return false;
  return true;
}

bool NMLoadStoreOpt::isCalleeSavedLoadStore(MachineInstr &MI, bool IsRestore) {
  unsigned Opcode = MI.getOpcode();
  if (IsRestore) {
    if (Opcode != Mips::LW_NM && Opcode != Mips::LWs9_NM)
      return false;
  } else {
    if (Opcode != Mips::SW_NM && Opcode != Mips::SWs9_NM)
      return false;
  }

  Register BaseReg = MI.getOperand(1).getReg();
  if (BaseReg != Mips::SP_NM)
    return false;

  // We care only for callee-saved registers.
  Register SrcReg = MI.getOperand(0).getReg();
  if (CalleeSaves.find(SrcReg) == CalleeSaves.end())
    return false;

  return true;
}

bool NMLoadStoreOpt::isReturn(MachineInstr &MI) {
  unsigned Opcode = MI.getOpcode();
  if (Opcode != Mips::PseudoReturnNM)
    return false;

  Register BaseReg = MI.getOperand(0).getReg();
  if (BaseReg != Mips::RA_NM)
    return false;

  return true;
}

bool NMLoadStoreOpt::isTailCall(MachineInstr &MI) {
  unsigned Opcode = MI.getOpcode();
  if (Opcode != Mips::TAILCALL_NM && Opcode != Mips::MUSTTAILCALL_NM &&
      Opcode != Mips::TAILCALLREG_NM && Opcode != Mips::MUSTTAILCALLREG_NM)
    return false;

  return true;
}

bool NMLoadStoreOpt::isMustTailCall(MachineInstr &MI) {
  unsigned Opcode = MI.getOpcode();
  return (Opcode == Mips::MUSTTAILCALL_NM ||
          Opcode == Mips::MUSTTAILCALLREG_NM);
}

void NMLoadStoreOpt::sortCalleeSavedLoadStoreList(InstrList &LoadStoreList) {
  // nanoMIPS save and restore instructions require callee-saved registers to be
  // saved in particular order on the stack. This sorts the list so that
  // registers are in correct order. But it is still necessary to check if the
  // offsets are valid (this requires sorted list).
  auto CompareInstructions = [this](MachineInstr *First, MachineInstr *Second) {
    Register FirstReg = First->getOperand(0).getReg();
    Register SecondReg = Second->getOperand(0).getReg();
    return CalleeSaves.at(FirstReg) < CalleeSaves.at(SecondReg);
  };
  std::sort(LoadStoreList.begin(), LoadStoreList.end(), CompareInstructions);
}

bool NMLoadStoreOpt::isExtensible(MachineInstr &MI, InstrList &LoadStoreList) {
  if (LoadStoreList.empty())
   return true;
  MachineInstr *LastMI = LoadStoreList.back();
  // Make sure that offsets are 4 bytes apart.
  int64_t CurrOffset = LastMI->getOperand(2).getImm();
  int64_t NextOffset = MI.getOperand(2).getImm();
  if (CurrOffset != NextOffset + 4)
    return false;

  // Make sure that there is no gap between registers.
  int64_t CurrReg = LastMI->getOperand(0).getReg();
  int64_t NextReg = MI.getOperand(0).getReg();
  if (CalleeSaves.at(CurrReg) != CalleeSaves.at(NextReg) - 1)
    return false;
  return true;
}

bool NMLoadStoreOpt::isRASaved(const InstrList &LoadStoreList) {
  return any_of(LoadStoreList, [](const MachineInstr* I) {
    return I->getOperand(0).getReg() == Mips::RA_NM;
  });
}

bool NMLoadStoreOpt::isValidSaveRestore16Offset(int64_t Offset) {
  return (Offset <= 240) && !(Offset & 0xf);
}

// Generates save or restore instruction.
//
// addiu $sp, $sp, -16  ->  save 16, $s1-$s4
// sw $s0, 12($sp)      ->
// sw $s1, 8($sp)       ->
// sw $s2, 4($sp)       ->
// sw $s3, 0($sp)       ->
//
// or:
//
// lw $s3, 0($sp)       -> restore.jrc 16, $s0-$s3
// lw $s2, 4($sp)       ->
// lw $s1, 8($sp)       ->
// lw $s0, 12($sp)      ->
// addiu $sp, $sp, 16   ->
// jrc $ra              ->
//
bool NMLoadStoreOpt::generateSaveOrRestore(MachineBasicBlock &MBB,
                                           bool IsRestore) {
  // Look for contiguous list of loads/stores.
  InstrList LoadStoreList;
  MachineInstr *AdjustStack = nullptr;
  MachineInstr *Return = nullptr;
  MachineInstr *TailCall = nullptr;

  if (IsRestore) {
    // Iterate bacbwards over BB in case were looking to generate restore,
    // because those instructions are at the end of the BB.
    for (auto &MI : make_range(MBB.rbegin(), MBB.rend())) {
      // There's no need to look for return, if we already found addiu. In case
      // return exists in this BB, it needs to be before addiu.
      if (!Return && !AdjustStack && isReturn(MI)) {
        Return = &MI;
        TailCall = nullptr;
        continue;
      }

      if (!TailCall && !AdjustStack && isTailCall(MI)) {
        TailCall = &MI;
        Return = nullptr;
        continue;
      }

      if (!AdjustStack && isStackPointerAdjustment(MI, IsRestore)) {
        AdjustStack = &MI;
        continue;
      }

      // There's no need to look for loads, if we haven't found addiu.
      if (!AdjustStack) {
        // Drop the return if sequence is broken before the addiu is found.
        if (!MI.isCFIInstruction() && !MI.isDebugInstr())
           Return = nullptr;
        continue;
      }
      // Since we are looking for a contguous list, we should stop searching for
      // more loads once the end of the list has been reached. Both return and
      // stack adjustment should be found by now, since we're iterating from the
      // end.
      if (   isCalleeSavedLoadStore(MI, IsRestore)
          && isExtensible(MI, LoadStoreList)) {
        LoadStoreList.emplace_back(&MI);
        continue;
      }

      // CFI and debug instructions don't break the sequence.
      if (MI.isCFIInstruction() || MI.isDebugInstr())
        continue;

      // Sequence has been broken, no need to continue. We either reached the
      // end or found nothing.
      if (!LoadStoreList.empty() || AdjustStack)
        break;
    }
  } else {
    for (auto &MI : MBB) {
      if (!AdjustStack && isStackPointerAdjustment(MI, IsRestore)) {
        AdjustStack = &MI;
        continue;
      }

      // There's no need to look for stores, if we haven't found addiu.
      if (!AdjustStack)
        continue;

      // Since we are looking for a contguous list, we should stop searching for
      // more stores once the end of the list has been reached.
      if (   isCalleeSavedLoadStore(MI, IsRestore)
          && isExtensible(MI, LoadStoreList)) {
        LoadStoreList.emplace_back(&MI);
        continue;
      }

      // CFI and debug instructions don't break the sequence.
      if (MI.isCFIInstruction() || MI.isDebugInstr())
        continue;

      // Sequence has been broken, no need to continue. We either reached the
      // end or found nothing.
      if (!LoadStoreList.empty() || AdjustStack)
        break;
    }
  }

  if (AdjustStack) {
    int64_t StackOffset = std::abs(AdjustStack->getOperand(2).getImm());
    // Stack offset has to be doubleword aligned and cannot be larger than 4092.
    // TODO: In case it is larger than 4092, we could emit addiu + save.
    if (StackOffset > 4092 || StackOffset & 0x7)
      return false;

    sortCalleeSavedLoadStoreList(LoadStoreList);

    // Save/restore instructions operate on the beginning of SP, but sometimes
    // that is allocated for function arguments. In that case, it is
    // neccessary to emit additional addiu (or save/restore) which adjusts stack
    // pointer to the place where callee-saves should be.
    //
    // addiu $sp, $sp, -32  ->  save 16
    // sw $s0, 12($sp)      ->  save 16, $s0-$s3
    // sw $s1, 8($sp)       ->
    // sw $s2, 4($sp)       ->
    // sw $s3, 0($sp)       ->
    //
    int64_t NewStackOffset = 0;
    if (!LoadStoreList.empty()) {
      auto LastOffset = LoadStoreList.front()->getOperand(2).getImm();
      assert(StackOffset >= LastOffset + 4);
      if (StackOffset > LastOffset + 4) {
        // LastOffset + 4 will be the new stack offset and needs to be
        // doubleword aligned. In case it is not, we need to remove an element
        // from the list in order to ensure the alignment.
        //
        // addiu $sp, $sp, -32  ->  save 16
        // sw $s0, 16($sp)      ->  save 16, $s1-$s4
        // sw $s1, 12($sp)      ->  sw $s0, 16($sp)
        // sw $s2, 8($sp)       ->
        // sw $s3, 4($sp)       ->
        // sw $s4, 0($sp)       ->
        //
        if ((LastOffset + 4) & 0x7) {
          LoadStoreList.erase(LoadStoreList.begin());
          // Since an element has been removed, it is neccessary to check
          // validity again.
          if (!LoadStoreList.empty()) {
            LastOffset = LoadStoreList.front()->getOperand(2).getImm();
            assert(!((LastOffset + 4) & 0x7));
            NewStackOffset = StackOffset - LastOffset - 4;
            StackOffset = LastOffset + 4;
          }
        } else {
          NewStackOffset = StackOffset - LastOffset - 4;
          StackOffset = LastOffset + 4;
        }
      }
    }

    if (LoadStoreList.empty()) {
      // Generate 16-bit save/restore if there are no register arguments or
      // register arguments are invalid. They have 8-bit offset with quadword
      // alignment.
      // FIXME: It is also neccessary to make sure not to generate restore
      // without registers, since it's an invalid instruction for GNU as.
      if (!isValidSaveRestore16Offset(StackOffset) || (IsRestore && !Return))
        return false;
    }

    if (TailCall && !isMustTailCall(*TailCall) &&
        MBB.getParent()->getFunction().hasOptSize() &&
        IsRestore &&
        !NewStackOffset &&
        isRASaved(LoadStoreList) &&
        isValidSaveRestore16Offset(StackOffset)) {
      assert(!Return);
      auto MII = BuildMI(MBB, std::next(MBBIter(TailCall)),
                          TailCall->getDebugLoc(), TII->get(Mips::RetRA));
      bool IsIndirect = TailCall->getOperand(0).isReg();
      TailCall->setDesc(TII->get(IsIndirect ? Mips::JALRCPseudo : Mips::BALC_NM));
      TailCall->addRegisterDefined(Mips::RA_NM);
      TailCall->addRegisterDefined(Mips::SP_NM);
      Return = MII.getInstr();
    }

    TailCall = nullptr;
    // If NewStackOffset is set, restore.jrc cannot be generated with Offset,
    // because it needs to be the last instruction in the basic block. If
    // possible, it will be generated with NewStackOffset.
    unsigned Opcode = IsRestore
                          ? ((Return && !NewStackOffset) ? Mips::RESTOREJRC16_NM
                                                         : Mips::RESTORE_NM)
                          : Mips::SAVE_NM;
    auto InsertBefore =
        std::next(MBBIter((Return && !NewStackOffset) ? Return : AdjustStack));
    auto DL = Return ? Return->getDebugLoc() : AdjustStack->getDebugLoc();
    auto MII = BuildMI(MBB, InsertBefore, DL, TII->get(Opcode));
    MII.addImm(StackOffset);
    MBB.erase(AdjustStack);

    for (size_t InsNo = 0; InsNo < LoadStoreList.size(); InsNo++) {
      MachineInstr *MI = LoadStoreList[InsNo];
      MII.addReg(MI->getOperand(0).getReg(),
                 IsRestore ? RegState::Define : RegState::Kill);
      MBB.erase(MI);
    }

    if (Return && !NewStackOffset) {
      // Copy return-value registers, if any.
      MII.copyImplicitOps(*Return);
      MBB.erase(Return);
    }

    if (NewStackOffset) {
      // NewStackOffset needs to be added before save, but after restore. This
      // is why iterator needs to go to the next iteration in case of restore.
      InsertBefore = MBBIter(MII.getInstr());
      if (IsRestore)
        InsertBefore = std::next(InsertBefore);
      // Favorable case is to generate save/restore.jrc (16-bit), but we have to
      // make sure the offset fits. Otherwise, we fall back to addiu (32-bit).
      // FIXME: It is also neccessary to make sure not to generate restore
      // without registers, since it's an invalid instruction for GNU as.
      if (isValidSaveRestore16Offset(NewStackOffset) &&
          (!IsRestore || (IsRestore && Return))) {
        if (Return) {
          // In case return is also consumed, we should put restore.jrc after
          // return, to make sure it is very last instruction.
          InsertBefore = std::next(MBBIter(Return));
          BuildMI(MBB, InsertBefore, DL, TII->get(Mips::RESTOREJRC16_NM))
              .addImm(NewStackOffset);
          MBB.erase(Return);
        } else {
          // Opcode can be safely reused.
          BuildMI(MBB, InsertBefore, DL, TII->get(Opcode))
              .addImm(NewStackOffset);
        }
      } else {
        // In case of save, the offset is subtracted from SP.
        if (!IsRestore)
          NewStackOffset = -NewStackOffset;
        BuildMI(MBB, InsertBefore, DL, TII->get(Mips::ADDIUNEG_NM), Mips::SP_NM)
            .addReg(Mips::SP_NM)
            .addImm(NewStackOffset);
      }
    }
    return true;
  }
  return false;
}

// Check if the instruction is lw, sw or addiu with Reg as second operand.
static bool isValidUse(MachineInstr *MI, Register Reg) {
  switch (MI->getOpcode()) {
  case Mips::ADDIU48_NM:
    if (!MI->getOperand(2).isImm())
      return false;
  LLVM_FALLTHROUGH;
  case Mips::SB_NM:
  case Mips::LB_NM:
  case Mips::LBU_NM:
  case Mips::SH_NM:
  case Mips::LH_NM:
  case Mips::LHU_NM:
  case Mips::SW_NM:
  case Mips::LW_NM:
  case Mips::SWs9_NM:
  case Mips::LWs9_NM:
  case Mips::SBs9_NM:
  case Mips::LBs9_NM:
  case Mips::LBUs9_NM:
  case Mips::SHs9_NM:
  case Mips::LHs9_NM:
  case Mips::LHUs9_NM:
  case Mips::ADDIU_NM:
  case Mips::ADDIUNEG_NM:
    return MI->getOperand(1).getReg() == Reg;
  default:
    return false;
  }
}

static bool isLoadStoreShortChar(MachineInstr *MI) {
  switch (MI->getOpcode()) {
  case Mips::SB_NM:
  case Mips::LB_NM:
  case Mips::LBU_NM:
  case Mips::SH_NM:
  case Mips::LH_NM:
  case Mips::LHU_NM:
    return true;
  default:
    return false;
  }
}

// Generates lwpc and swpc or improves la instruction. Replacement happens only
// if there is just a single use of 'symbol'. If it's used by multiple
// instructions, then the replacement would end up being a regression.
//
// la $a0, symbol       -> lwpc $a1, symbol+4
// lw $a1, 4($a0)
//
// or:
//
// la $a0, symbol       -> swpc $a1, symbol
// sw $a1, 0($a0)
//
// or:
//
// la $a0, symbol       -> la $a1, symbol+8
// addiu $a1, $a0, 8
//
// or:
//
// la $a0, symbol       -> aluipc $a0, %pcrel_hi(symbol+2)
// lh $a1, 2($a0)       -> lh     $a1, lo(symbol+2)($a0)
//
bool NMLoadStoreOpt::generatePCRelative(MachineBasicBlock &MBB) {
  SmallVector<std::pair<MachineInstr *, MachineInstr *>> Candidates;
  SmallVector<MachineInstr *> Unused;
  for (auto &MI : MBB) {
    if (MI.getOpcode() == Mips::LAPC48_NM ||
	MI.getOpcode() == Mips::PseudoLA_NM) {
      bool IsRedefined = false;
      bool IsUsedByMultipleMIs = false;
      MachineInstr *FirstUse = nullptr;
      Register Dst = MI.getOperand(0).getReg();

      // Iterate over all of the remaining instructions in the basic block and
      // count how many times the result of LA has been used. If it has been
      // used more than once, then it doesn't pay off to replace it. This loop
      // also checks if the register used by LA has been reused, this stops the
      // search.
      for (auto &MI2 : make_range(std::next(MBBIter(MI)), MBB.end())) {
        for (auto &Opnd : MI2.operands()) {
          if (Opnd.isReg() && Opnd.isUse() && Opnd.getReg() == Dst) {
            if (FirstUse == nullptr) {
              FirstUse = &MI2;
              continue;
            } else {
              IsUsedByMultipleMIs = true;
              break;
            }
          }
          if (Opnd.isReg() && Opnd.isDef() && Opnd.getReg() == Dst)
            IsRedefined = true;
        }
        if (IsUsedByMultipleMIs || IsRedefined)
          break;
      }

      // Used by multiple instructions, too expensive for replacement.
      if (IsUsedByMultipleMIs)
        continue;

      // Check if the register used by LA is live-in for the successor basic
      // blocks. In case it is, this means that it used by those basic block and
      // that it probably doesn't pay off to replace it. If the register has
      // been redefined in the current basic block then the check makes no sense
      // anymore.
      bool IsUsedInOtherBBs = false;
      if (!IsRedefined)
        for (auto *Succ : MBB.successors())
          if (Succ->isLiveIn(Dst)) {
            IsUsedInOtherBBs = true;
            break;
          }

      // Used by other basic block, this usually (not always though) means that
      // it's used by multiple instructions.
      if (IsUsedInOtherBBs)
        continue;

      if (FirstUse == nullptr) {
        // The LA is unused and dead.
        Unused.push_back(&MI);
      } else {

        assert(!IsUsedByMultipleMIs && FirstUse != nullptr);

        if (!isValidUse(FirstUse, Dst))
          continue;

        Candidates.push_back({&MI, FirstUse});
      }
    }
  }

  for (MachineInstr *MI : Unused)
    MBB.erase(MI);

  for (auto Pair : Candidates) {
    auto *LA = Pair.first;
    auto *Use = Pair.second;
    auto &Address = LA->getOperand((LA->getOpcode() == Mips::LAPC48_NM ||
				    LA->getOpcode() == Mips::LI48_NM ||
				    LA->getOpcode() == Mips::PseudoLA_NM)? 1 : 2);
    auto Dst = Use->getOperand(0).getReg();
    int64_t Offset = Use->getOperand(2).getImm() + Address.getOffset();

    assert(Address.isGlobal());

    if (Use->getOpcode() == Mips::ADDIU_NM ||
	Use->getOpcode() == Mips::ADDIUNEG_NM ||
	Use->getOpcode() == Mips::ADDIU48_NM) {
      // Move LA to its use to avoid extending the lifetime of Dst
      MBB.insert(MBBIter(Use),
                 MBB.remove(LA));
      Address.setOffset(Offset);
      LA->getOperand(0).setReg(Dst);
      MBB.erase(Use);
    } else if (isLoadStoreShortChar(Use)) {
      auto InsertBefore = std::next(MBBIter(LA));
      BuildMI(MBB, InsertBefore, Use->getDebugLoc(), TII->get(Mips::ALUIPC_NM))
          .addReg(LA->getOperand(0).getReg(), RegState::Define)
          .addGlobalAddress(Address.getGlobal(), Offset, MipsII::MO_PCREL_HI);

      Use->getOperand(2).ChangeToGA(Address.getGlobal(), Offset,
                                    MipsII::MO_ABS_LO);
      MBB.erase(LA);
    } else {
      auto InsertBefore = std::next(MBBIter(Use));
      bool IsLoad = Use->mayLoad();
      unsigned Opcode = IsLoad ? Mips::LWPC_NM : Mips::SWPC_NM;
      unsigned Flags = Address.getTargetFlags();
      BuildMI(MBB, InsertBefore, Use->getDebugLoc(), TII->get(Opcode))
          .addReg(Dst, IsLoad ? RegState::Define : 0)
          .addGlobalAddress(Address.getGlobal(), Offset, Flags);
      MBB.erase(LA);
      MBB.erase(Use);
    }
  }

  return Candidates.size() > 0;
}

INITIALIZE_PASS(NMLoadStoreOpt, DEBUG_TYPE, PASS_NAME, false, false)

namespace llvm {
FunctionPass *createNanoMipsLoadStoreOptimizerPass() {
  return new NMLoadStoreOpt();
}
} // namespace llvm
