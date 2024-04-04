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
#include "llvm/InitializePasses.h"

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
  bool compressJumpTable(MachineInstr &MI);
  bool optimizeRedundantEntries(MachineBasicBlock::iterator &I);

  NMOptimizeJumpTables() : MachineFunctionPass(ID) {
    initializeNMOptimizeJumpTablesPass(*PassRegistry::getPassRegistry());
  }
  StringRef getPassName() const override {
    return NM_OPTIMIZE_JUMP_TABLES_OPT_NAME;
  }
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

INITIALIZE_PASS_BEGIN(NMOptimizeJumpTables, "nm-optimize-jt", NM_OPTIMIZE_JUMP_TABLES_OPT_NAME,
                      false, false)
INITIALIZE_PASS_DEPENDENCY(MachineBlockFrequencyInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MachineBranchProbabilityInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ProfileSummaryInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MachineLoopInfoWrapperPass)
INITIALIZE_PASS_END(NMOptimizeJumpTables, "nm-optimize-jt", NM_OPTIMIZE_JUMP_TABLES_OPT_NAME,
                    false, false)

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
  auto JTOp = (*I).getOperand(3);
  int JTIdx = JTOp.getIndex();
  auto &JTInfo = *MF->getJumpTableInfo();
  const MachineJumpTableEntry &JT = JTInfo.getJumpTables()[JTIdx];
  llvm::SmallPtrSet<MachineBasicBlock *, 16> JTMBBS;

  // Collect all different JT MBBs.
  for (auto MBB : JT.MBBs)
    JTMBBS.insert(MBB);

  MachineBasicBlock *MBBToJumpTo = nullptr;
  MachineBasicBlock *CurrBB = JTOp.getParent()->getParent();

  for (auto *MBB : JTMBBS) {
    auto I = MBB->getFirstNonDebugInstr();
    while (I == MBB->end() || I->isUnconditionalBranch()) {
      // Empty block. Forward.
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

  // Optimize JT and corresponding instructions. First, we want to replace
  // LoadJumpTableOffset and BRSC_NM with an unconditional jump to MBBToJumpTo.
  // MBBToJumpTo would be the only nonempty block.
  // Insert new BC_NM instruction after LoadJumpTableOffset.
  BuildMI(*CurrBB, I, JTOp.getParent()->getDebugLoc(), TII->get(Mips::BC_NM))
      .addMBB(MBBToJumpTo);
  --I;
  SmallVector<MachineInstr *, 3> InstrsToDelete;

  // We want to delete all unnecessary instructions after removing
  // LoadJumpTableOffset.
  for (auto &MBB : *(CurrBB->getParent()))
   for (auto &I : MBB) {  
    for (llvm::MachineInstr::mop_iterator OpI = I.operands_begin(),
                                          OpEnd = I.operands_end();
         OpI != OpEnd; ++OpI) {
      llvm::MachineOperand &Operand = *OpI;
      if (Operand.isIdenticalTo(JTOp))
        InstrsToDelete.push_back(&I);
    }
   }

  for (auto Instr : InstrsToDelete)
    Instr->removeFromParent();
  return true;
}

bool NMOptimizeJumpTables::compressJumpTable(MachineInstr &MI) {
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
    for (MachineBasicBlock::iterator I = MBB.begin(), E = MBB.end(); I != E;
         ++I) {
      MachineInstr &MI = *I;
      if (MI.getOpcode() != Mips::LoadJumpTableOffset)
        continue;
      bool OptimizedJT = optimizeRedundantEntries(I);
      CleanUpNeeded |= OptimizedJT;
      Modified |= OptimizedJT;
      if (!OptimizedJT)
        Modified |= compressJumpTable(MI);
    }
  }
  if (CleanUpNeeded) {
    MBFIWrapper MBFI(getAnalysis<MachineBlockFrequencyInfoWrapperPass>().getMBFI());

    const MachineBranchProbabilityInfo *MBPI =
        &getAnalysis<MachineBranchProbabilityInfoWrapperPass>().getMBPI();

    ProfileSummaryInfo *PSI =
        &getAnalysis<ProfileSummaryInfoWrapperPass>().getPSI();

    auto MLI = &getAnalysis<MachineLoopInfoWrapperPass>().getLI();

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
