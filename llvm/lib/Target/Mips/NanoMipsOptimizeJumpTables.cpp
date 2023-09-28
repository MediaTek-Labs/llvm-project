//===- NanoMipsOptimizeJumpTables.cpp - nanoMIPS optimize JTs  --------===//
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
/// removes any associated code. It also compresses the Jump Table entries,
/// whenever possible. In the past, jump table entries used to be fixed size
/// (4B) and represented absolute addresses. However, we can now compress those
/// entries by filling them with specific offsets. This helps save at least 2B
/// per entry. The optimization pass checks if one or two bytes are sufficient
/// for the offset value.
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

#include <cmath>

using namespace llvm;

#define NM_OPTIMIZE_JUMP_TABLES_OPT_NAME                                       \
  "nanoMIPS jump table optimization pass"

static cl::opt<bool> NMForce16BitJumpTables(
    "nmips-force-16-bit-jump-table", cl::Hidden, cl::init(true),
    cl::desc("Force all jump tables to have 16bit wide entries"));

namespace {
struct NMOptimizeJumpTables : public MachineFunctionPass {
  static char ID;
  const MipsSubtarget *STI;
  const TargetInstrInfo *TII;
  MachineFunction *MF;
  SmallVector<int, 8> BlockInfo;
  SmallVector<int, 8> BrOffsets;

  int computeBlockSize(MachineBasicBlock &MBB);
  void scanFunction();
  bool compressJumpTable(MachineInstr &MI, int Offset);
  bool optimizeRedundantEntries(MachineBasicBlock::iterator &I);

  NMOptimizeJumpTables() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override {
    return NM_OPTIMIZE_JUMP_TABLES_OPT_NAME;
  }
  bool runOnMachineFunction(MachineFunction &Fn) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<MachineBlockFrequencyInfo>();
    AU.addRequired<MachineBranchProbabilityInfo>();
    AU.addRequired<ProfileSummaryInfoWrapperPass>();
    AU.addRequired<MachineLoopInfo>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
};
} // namespace

char NMOptimizeJumpTables::ID = 0;

// TODO: Currently, there is no existing LLVM interface which we can use to tell the
// maximum potential size of a MachineInstr. Once we have it, this should be
// enhanced.
int NMOptimizeJumpTables::computeBlockSize(MachineBasicBlock &MBB) {
  int Size = 0;
  for (const MachineInstr &MI : MBB)
    Size += TII->getInstSizeInBytes(MI);
  return Size;
}

void NMOptimizeJumpTables::scanFunction() {
  BlockInfo.clear();
  BlockInfo.resize(MF->getNumBlockIDs());
  BrOffsets.clear();
  bool findBR = MF->getJumpTableInfo() &&
                !MF->getJumpTableInfo()->getJumpTables().empty();
  if (findBR)
    BrOffsets.resize(MF->getJumpTableInfo()->getJumpTables().size());
  int Offset = 0;
  for (MachineBasicBlock &MBB : *MF) {
    BlockInfo[MBB.getNumber()] = Offset;
    Offset += computeBlockSize(MBB);
    if (findBR)
      for (auto &MI : MBB) {
        if (MI.getOpcode() == Mips::BRSC_NM) {
          int JTIdx = MI.getOperand(1).getIndex();
          BrOffsets[JTIdx] = Offset;
          break;
        }
      }
  }
}

bool NMOptimizeJumpTables::optimizeRedundantEntries(
    MachineBasicBlock::iterator &I) {
  MachineInstr &MI = *I;
  int JTIdx = MI.getOperand(3).getIndex();
  auto &JTInfo = *MF->getJumpTableInfo();
  const MachineJumpTableEntry &JT = JTInfo.getJumpTables()[JTIdx];
  llvm::SmallPtrSet<MachineBasicBlock *, 16> JTMBBS;

  // Collect all different JT MBBs.
  for (auto MBB : JT.MBBs)
    JTMBBS.insert(MBB);

  bool NonEmptyBlock = false;
  for (auto *MBB : JTMBBS) {
    auto I = MBB->getFirstNonDebugInstr();
    // Check if the block is empty, is only reachable through the jump table,
    // and its successor is already in JT MBBs. If so, remove it from JT MBBs
    // set.
    if ((I == MBB->end() || I->isUnconditionalBranch()) &&
        (MBB->pred_size() == 1 && MBB->succ_size() == 1) &&
        (JTMBBS.contains(*MBB->successors().begin()))) {
      JTMBBS.erase(MBB);
    } else {
      // Nonempty block. If this is the second nonempty, we can't optimize JT.
      if (NonEmptyBlock)
        break;
      NonEmptyBlock = true;
    }
  }
  if (JTMBBS.size() != 1)
    return false;
  // Optimize JT and corresponding instructions. First, we want to replace
  // LoadJumpTableOffset and BRSC_NM with an unconditional jump to MBBToJumpTo.
  // MBBToJumpTo would be either the only nonempty block or the only possible
  // target if all JT MBBs were equal.
  auto MBBToJumpTo = (*(JTMBBS.begin()));
  MachineBasicBlock *CurrBB = MI.getParent();

  // Insert new BC_NM instruction after LoadJumpTableOffset.
  MachineInstrBuilder MIB =
      BuildMI(*CurrBB, I, MI.getDebugLoc(), TII->get(Mips::BC_NM));
  MIB.add(MachineOperand::CreateMBB(MBBToJumpTo));
  --I;
  SmallVector<MachineInstr *, 3> InstrsToDelete;

  // We want to delete all unnecessary instructions after removing
  // LoadJumpTableOffset.
  for (auto &I : *CurrBB) {
    for (llvm::MachineInstr::mop_iterator OpI = I.operands_begin(),
                                          OpEnd = I.operands_end();
         OpI != OpEnd; ++OpI) {
      llvm::MachineOperand &Operand = *OpI;
      if (Operand.isIdenticalTo(MI.getOperand(3)))
        InstrsToDelete.push_back(&I);
    }
  }
  for (size_t i = 0; i < InstrsToDelete.size(); i++)
    InstrsToDelete[i]->removeFromParent();
  return true;
}

bool NMOptimizeJumpTables::compressJumpTable(MachineInstr &MI, int Offset) {
  int JTIdx = MI.getOperand(3).getIndex();
  auto &JTInfo = *MF->getJumpTableInfo();
  const MachineJumpTableEntry &JT = JTInfo.getJumpTables()[JTIdx];

  // The jump-table might have been optimized away.
  if (JT.MBBs.empty())
    return false;

  int MaxOffset = std::numeric_limits<int>::min(),
      MinOffset = std::numeric_limits<int>::max();
  int BrOffset = BrOffsets[JTIdx];

  bool Signed = false;
  for (auto Block : JT.MBBs) {
    int BlockOffset = BlockInfo[Block->getNumber()];
    MaxOffset = std::max(MaxOffset, BlockOffset - BrOffset);
    MinOffset = std::min(MinOffset, BlockOffset - BrOffset);
  }
  if (MinOffset < 0)
    Signed = true;

  if (std::max(std::abs(MinOffset), MaxOffset) == MinOffset)
    MaxOffset = MinOffset;

  auto MFI = MF->getInfo<MipsFunctionInfo>();
  MCSymbol *JTS = MFI->getJumpTableSymbol(JTIdx);

  if (NMForce16BitJumpTables) {
    MFI->setJumpTableEntryInfo(JTIdx, 2, JTS, Signed);
    return false;
  }

  bool EntrySize1 =
      (Signed && isInt<8>(MaxOffset)) || (!Signed && isUInt<8>(MaxOffset));
  bool EntrySize2 =
      (Signed && isInt<16>(MaxOffset)) || (!Signed && isUInt<16>(MaxOffset));
  int EntrySize = EntrySize1 ? 1 : (EntrySize2 ? 2 : 4);
  if (EntrySize1 || EntrySize2)
    MFI->setJumpTableEntryInfo(JTIdx, EntrySize, JTS, Signed);

  return false;
}

bool NMOptimizeJumpTables::runOnMachineFunction(MachineFunction &Fn) {
  STI = &static_cast<const MipsSubtarget &>(Fn.getSubtarget());
  TII = STI->getInstrInfo();
  bool Modified = false;
  MF = &Fn;

  scanFunction();

  bool CleanUpNeeded = false;
  for (MachineBasicBlock &MBB : *MF) {
    int Offset = BlockInfo[MBB.getNumber()];
    for (MachineBasicBlock::iterator I = MBB.begin(), E = MBB.end(); I != E;
         ++I) {
      MachineInstr &MI = *I;
      if (MI.getOpcode() != Mips::LoadJumpTableOffset)
        continue;
      bool OptimizedJT = optimizeRedundantEntries(I);
      CleanUpNeeded |= OptimizedJT;
      Modified |= OptimizedJT;
      if (!OptimizedJT)
        Modified |= compressJumpTable(MI, Offset);
      Offset += TII->getInstSizeInBytes(MI);
    }
  }
  if (CleanUpNeeded) {
    MBFIWrapper MBFI(getAnalysis<MachineBlockFrequencyInfo>());
    const MachineBranchProbabilityInfo *MBPI =
        &getAnalysis<MachineBranchProbabilityInfo>();
    ProfileSummaryInfo *PSI =
        &getAnalysis<ProfileSummaryInfoWrapperPass>().getPSI();
    auto MLI = &getAnalysis<MachineLoopInfo>();

    BranchFolder BF(true, false, MBFI, *MBPI, PSI);
    // We need a cleanup here, even though the pass runs after
    // MachineBlockPlacement.
    BF.OptimizeFunction(*MF, TII, MF->getSubtarget().getRegisterInfo(), MLI,
                        /*AfterPlacement=*/false);
  }
  return Modified;
}

namespace llvm {
FunctionPass *createNanoMipsOptimizeJumpTablesPass() {
  return new NMOptimizeJumpTables();
}
} // namespace llvm
