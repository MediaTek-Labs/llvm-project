//===--------------- lib/Target/Mips/NanoMipsTrapOpt.cpp -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This pass optimizes trap instructions at the generic IR level
//
//===----------------------------------------------------------------------===//

#include "NanoMipsTrapOpt.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicsMips.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PatternMatch.h"

#define DEBUG_TYPE "nmips-trap-optimizer"

using namespace llvm;
using namespace llvm::PatternMatch;

static cl::opt<bool>
    DisableCondtrap("disable-nmips-condtrap", cl::Hidden, cl::init(true),
                    cl::desc("Disable nanoMIPS conditional trap generation"));

extern cl::opt<int> NMDebugTrapCode;

extern cl::opt<int> NMUBSanTrapCode;

static cl::opt<bool> EnableAddtrap(
    "enable-nmips-addsubtrap", cl::Hidden, cl::init(false),
    cl::desc(
        "Enable nanoMIPS conditional trapping-on-overflow add/sub generation"));

// find pattern:
//   %packed = call { i32, i1 } @llvm.sadd.with.overflow
//   %cond = extractvalue { i32, i1 } %packed, 1
//   br i1 %cond, label %trap, label %notrap
//   ...
//   %result =  extractvalue { i32, i1 } %packed, 0
// eliminate branching to %trap, and replace %result with:
//   %result = call i32 @llvm.mips.sadd.trap.on.overflow
bool NanoMipsTrapOptPass::internalizeTrapOfAddSub(Function &F, Value *Cond) {
  Value *Packed = nullptr;
  bool Match = match(Cond, m_ExtractValue<1>(m_Value(Packed)));
  if (Match) {
    Value *LHS = nullptr, *RHS = nullptr;
    Module *M = F.getParent();
    auto internalizeTrap = [&](bool isAdd) {
      if (isAdd) {
        Match = match(Packed, m_Intrinsic<Intrinsic::sadd_with_overflow>(
                                  m_Value(LHS), m_Value(RHS)));
      } else {
        Match = match(Packed, m_Intrinsic<Intrinsic::ssub_with_overflow>(
                                  m_Value(LHS), m_Value(RHS)));
      }
      if (Match && LHS->getType()->isIntegerTy(32) &&
          RHS->getType()->isIntegerTy(32)) {
        CallInst *CI = cast<CallInst>(Packed);
        SmallVector<User *, 4> Users;
        for (auto *U : CI->users())
          if (match(U, m_ExtractValue<0>(m_Value())))
            Users.push_back(U);
        if (Users.size()) {
          Intrinsic::ID ReplacementID =
              isAdd ? Intrinsic::mips_sadd_trap_on_overflow
                    : Intrinsic::mips_ssub_trap_on_overflow;
          Function *F =
              Intrinsic::getDeclaration(M, ReplacementID, LHS->getType());
          CallInst *TrappingOp = CallInst::Create(F, {LHS, RHS}, "", CI);
          for (auto *U : Users)
            U->replaceAllUsesWith(TrappingOp);
          return true;
        }
      }
      return false;
    };
    if (internalizeTrap(true /* Add */))
      return true;
    if (internalizeTrap(false /* Sub */))
      return true;
  }
  return false;
}

// optimize traps:
//  1. internalize traps into the triggering operation,
//     depending on hardware support
//  2. emit a conditional trap instead of branching to one
PreservedAnalyses NanoMipsTrapOptPass::run(Function &F,
                                           FunctionAnalysisManager &AM) {
  bool MadeChange = false;
  std::vector<CallInst *> Traps;
  Module *M = F.getParent();
  auto storeTraps = [&](Intrinsic::ID IID) {
    if (auto *Trap = M->getFunction(Intrinsic::getName(IID))) {
      // Find uses of the Trap intrinsic in this function by checking
      // instructions in blocks which end in 'unreachable'
      for (auto &BB : F)
        if (isa<UnreachableInst>(BB.getTerminator())) {
          for (auto &I : BB)
            if (auto *CI = llvm::dyn_cast<llvm::CallInst>(&I))
              if (CI->getCalledFunction() == Trap)
                Traps.push_back(CI);
        }
    }
  };
  storeTraps(Intrinsic::debugtrap);
  storeTraps(Intrinsic::ubsantrap);
  for (auto TI : Traps) {
    for (auto BB : llvm::make_early_inc_range(predecessors(TI->getParent()))) {
      BranchInst *BI = dyn_cast<BranchInst>(BB->getTerminator());
      if (!BI || !BI->isConditional())
        continue;

      bool IsTrue = BI->getSuccessor(0) == TI->getParent();

      auto Cond = BI->getCondition();
      if (EnableAddtrap && internalizeTrapOfAddSub(F, Cond)) {
        TI->getParent()->removePredecessor(BB);
        BasicBlock *Succ = BI->getSuccessor(IsTrue ? 1 : 0);
        BranchInst::Create(Succ, BB);
        BI->eraseFromParent();
        MadeChange = true;
        continue;
      }
      if (DisableCondtrap)
        continue;

      TI->getParent()->removePredecessor(BB);
      if (!IsTrue) {
        Cond = BinaryOperator::CreateXor(
            Cond, ConstantInt::getTrue(Cond->getContext()), "", BI);
      }

      Cond = CastInst::CreateZExtOrBitCast(
          Cond, Type::getInt32Ty(Cond->getContext()), "", BI);

      Value *Args[2] = {
          Cond, ConstantInt::get(Type::getInt8Ty(Cond->getContext()),
                                 (TI->getIntrinsicID() == Intrinsic::debugtrap
                                      ? NMDebugTrapCode
                                      : NMUBSanTrapCode) &
                                     maskTrailingOnes<uint32_t>(5))};
      CallInst::Create(Intrinsic::getDeclaration(M, Intrinsic::mips_condtrap),
                       Args, "", BI);
      BranchInst::Create(BI->getSuccessor(IsTrue), BI);
      BI->eraseFromParent();
      MadeChange = true;
    }
  }
  return MadeChange ? PreservedAnalyses::none() : PreservedAnalyses::all();
}
