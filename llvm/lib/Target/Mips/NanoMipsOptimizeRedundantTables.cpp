//===- NanoMipsOptimizeRedundantTables.cpp - nanoMIPS opt redundant JTs --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file This file contains an optimization pass related to jump tables. Some
/// jump tables may not have any effect on the address of the following branch,
/// making them redundant. This optimization pass identifies such tables and
/// removes any associated code.
//===----------------------------------------------------------------------===//

#include "Mips.h"
#include "MipsMachineFunction.h"
#include "MipsSubtarget.h"
#include "llvm/Analysis/ProfileSummaryInfo.h"
#include "llvm/CodeGen/BranchFolding.h"
#include "llvm/CodeGen/MBFIWrapper.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineBlockFrequencyInfo.h"
#include "llvm/CodeGen/MachineBranchProbabilityInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/InitializePasses.h"

using namespace llvm;

#define NM_RED_JUMP_TABLES_OPT_NAME                                            \
  "nanoMIPS redundant jump table optimization pass"

namespace {
struct NMRedundantJumpTables : public MachineFunctionPass {
  static char ID;
  const MipsSubtarget *STI;
  const TargetInstrInfo *TII;
  MachineFunction *MF;
  SmallPtrSet<MachineInstr *, 16> InstrsToDelete;

  bool optimizeRedundantEntries(MachineInstr &MI);
  bool tryRedundantTableElimination();

  NMRedundantJumpTables() : MachineFunctionPass(ID) {
    initializeNMRedundantJumpTablesPass(*PassRegistry::getPassRegistry());
  }
  StringRef getPassName() const override { return NM_RED_JUMP_TABLES_OPT_NAME; }
  bool runOnMachineFunction(MachineFunction &Fn) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<MachineBlockFrequencyInfoWrapperPass>();
    AU.addRequired<MachineBranchProbabilityInfoWrapperPass>();
    AU.addRequired<ProfileSummaryInfoWrapperPass>();
    AU.addRequired<MachineLoopInfoWrapperPass>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
};
} // namespace

INITIALIZE_PASS_BEGIN(NMRedundantJumpTables, "nm-redundant-jt",
                      NM_RED_JUMP_TABLES_OPT_NAME, false, false)
INITIALIZE_PASS_DEPENDENCY(MachineBlockFrequencyInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MachineBranchProbabilityInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ProfileSummaryInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MachineLoopInfoWrapperPass)
INITIALIZE_PASS_END(NMRedundantJumpTables, "nm-redundant-jt",
                    NM_RED_JUMP_TABLES_OPT_NAME, false, false)

char NMRedundantJumpTables::ID = 0;

bool NMRedundantJumpTables::optimizeRedundantEntries(MachineInstr &MI) {
  auto JTOp = MI.getOperand(3);
  int JTIdx = JTOp.getIndex();
  auto &JTInfo = *MF->getJumpTableInfo();
  const MachineJumpTableEntry &JT = JTInfo.getJumpTables()[JTIdx];
  llvm::SmallPtrSet<MachineBasicBlock *, 16> JTMBBS;

  // The jump-table might have been optimized away.
  if (JT.MBBs.empty())
    return false;

  // Collect all different JT MBBs.
  for (auto MBB : JT.MBBs)
    JTMBBS.insert(MBB);

  MachineBasicBlock *MBBToJumpTo = nullptr;
  MachineBasicBlock *CurrBB = JTOp.getParent()->getParent();

  for (auto *MBB : JTMBBS) {
    auto I = MBB->getFirstNonDebugInstr();
    while (I == MBB->end() || I->isUnconditionalBranch()) {
      // Empty block. Forward.
      if (MBB->succ_empty())
        break;
      MBB = *MBB->successors().begin();
      I = MBB->getFirstNonDebugInstr();
    }
    if (!MBBToJumpTo)
      MBBToJumpTo = MBB;
    if (MBBToJumpTo != MBB)
      return false;
  }

  // If we are not able to find a block to jump to, it implies JT with empty
  // MBBs - an error in this case.
  assert(MBBToJumpTo && "Empty Jump Table.");
  // Optimize JT and corresponding instructions. First, we want to replace every
  // LoadJumpTableOffset and BRSC_NM with an unconditional jump to MBBToJumpTo.
  // MBBToJumpTo would be the only nonempty block.
  // We want to replace every BRSC_NM with a new BC_NM instruction, and to
  // delete all unnecessary instructions.
  for (auto &MBB : *(CurrBB->getParent()))
    for (auto &I : MBB) {
      for (llvm::MachineInstr::mop_iterator OpI = I.operands_begin(),
                                            OpEnd = I.operands_end();
           OpI != OpEnd; ++OpI) {
        llvm::MachineOperand &Operand = *OpI;
        if (!Operand.isIdenticalTo(JTOp))
          continue;
        if (I.getOpcode() == Mips::BRSC_NM)
          BuildMI(MBB, I, Operand.getParent()->getDebugLoc(),
                  TII->get(Mips::BC_NM))
              .addMBB(MBBToJumpTo);
        InstrsToDelete.insert(&I);
      }
    }

  // Remove leftover successors of the original jump table
  SmallVector<MachineBasicBlock*, 4> ToRemove;
  for (auto *Succ : CurrBB->successors()) {
    if (Succ != MBBToJumpTo && !Succ->isEHPad())
      ToRemove.push_back(Succ);
  }
  for (auto *Succ : ToRemove)
    CurrBB->removeSuccessor(Succ);
  
  // Mark JT as dead.
  JTInfo.RemoveJumpTable(JTIdx);
  return true;
}

bool NMRedundantJumpTables::tryRedundantTableElimination() {
  InstrsToDelete.clear();
  bool CleanUpNeeded = false;
  for (MachineBasicBlock &MBB : *MF) {
    for (auto &MI : MBB) {
      if (MI.getOpcode() != Mips::LoadJumpTableOffset)
        continue;
      CleanUpNeeded |= optimizeRedundantEntries(MI);
    }
  }
  if (!CleanUpNeeded)
    return false;

  // Now is safe to remove marked instructions. Clean up.
  for (auto Instr : InstrsToDelete)
    Instr->eraseFromParent();

  MBFIWrapper MBFI(getAnalysis<MachineBlockFrequencyInfoWrapperPass>().getMBFI());
  const MachineBranchProbabilityInfo *MBPI =
      &getAnalysis<MachineBranchProbabilityInfoWrapperPass>().getMBPI();
  ProfileSummaryInfo *PSI =
      &getAnalysis<ProfileSummaryInfoWrapperPass>().getPSI();
  auto MLI = &getAnalysis<MachineLoopInfoWrapperPass>().getLI();

  BranchFolder BF(true, false, MBFI, *MBPI, PSI);
  BF.OptimizeFunction(*MF, TII, MF->getSubtarget().getRegisterInfo(), MLI,
                      /*AfterPlacement=*/false);
  return true;
}

bool NMRedundantJumpTables::runOnMachineFunction(MachineFunction &Fn) {
  STI = &static_cast<const MipsSubtarget &>(Fn.getSubtarget());
  TII = STI->getInstrInfo();
  MF = &Fn;
  return tryRedundantTableElimination();
}

namespace llvm {
FunctionPass *createNanoMipsOptimizeRedundantJumpTablesPass() {
  return new NMRedundantJumpTables();
}
} // namespace llvm
