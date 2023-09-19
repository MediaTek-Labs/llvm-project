//===- JumpThreadingUtils.cpp - JumpThreading Utils -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This files implements utilities useful for all passes that perform
// jumpthreading-like transformation
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/JumpThreadingUtils.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/ProfDataUtils.h"
#include "llvm/Support/BranchProbability.h"

using namespace llvm;
using namespace jumpthreading;

void llvm::updatePredecessorProfileMetadata(const PredValueInfo &PredValues,
                                            BasicBlock *BB) {
  BranchInst *CondBr = dyn_cast<BranchInst>(BB->getTerminator());
  if (!CondBr)
    return;

  uint64_t TrueWeight, FalseWeight;
  if (!extractBranchWeights(*CondBr, TrueWeight, FalseWeight))
    return;

  if (TrueWeight + FalseWeight == 0)
    // Zero branch_weights do not give a hint for getting branch probabilities.
    // Technically it would result in division by zero denominator, which is
    // TrueWeight + FalseWeight.
    return;

  // Returns the outgoing edge of the dominating predecessor block
  // that leads to the PhiNode's incoming block:
  auto GetPredOutEdge =
      [](BasicBlock *IncomingBB,
         BasicBlock *PhiBB) -> std::pair<BasicBlock *, BasicBlock *> {
    auto *PredBB = IncomingBB;
    auto *SuccBB = PhiBB;
    SmallPtrSet<BasicBlock *, 16> Visited;
    while (true) {
      BranchInst *PredBr = dyn_cast<BranchInst>(PredBB->getTerminator());
      if (PredBr && PredBr->isConditional())
        return {PredBB, SuccBB};
      Visited.insert(PredBB);
      auto *SinglePredBB = PredBB->getSinglePredecessor();
      if (!SinglePredBB)
        return {nullptr, nullptr};

      // Stop searching when SinglePredBB has been visited. It means we see
      // an unreachable loop.
      if (Visited.count(SinglePredBB))
        return {nullptr, nullptr};

      SuccBB = PredBB;
      PredBB = SinglePredBB;
    }
  };

  for (const auto &PredValue : PredValues) {
    ConstantInt *CI = dyn_cast<ConstantInt>(PredValue.first);

    if (!CI || !CI->getType()->isIntegerTy(1))
      continue;

    BranchProbability BP =
        (CI->isOne() ? BranchProbability::getBranchProbability(
                           TrueWeight, TrueWeight + FalseWeight)
                     : BranchProbability::getBranchProbability(
                           FalseWeight, TrueWeight + FalseWeight));

    auto PredOutEdge = GetPredOutEdge(PredValue.second, BB);
    if (!PredOutEdge.first)
      continue;

    BasicBlock *PredBB = PredOutEdge.first;
    BranchInst *PredBr = dyn_cast<BranchInst>(PredBB->getTerminator());
    if (!PredBr)
      continue;

    uint64_t PredTrueWeight, PredFalseWeight;
    // FIXME: We currently only set the profile data when it is missing.
    // With PGO, this can be used to refine even existing profile data with
    // context information. This needs to be done after more performance
    // testing.
    if (extractBranchWeights(*PredBr, PredTrueWeight, PredFalseWeight))
      continue;

    // We can not infer anything useful when BP >= 50%, because BP is the
    // upper bound probability value.
    if (BP >= BranchProbability(50, 100))
      continue;

    SmallVector<uint32_t, 2> Weights;
    if (PredBr->getSuccessor(0) == PredOutEdge.second) {
      Weights.push_back(BP.getNumerator());
      Weights.push_back(BP.getCompl().getNumerator());
    } else {
      Weights.push_back(BP.getCompl().getNumerator());
      Weights.push_back(BP.getNumerator());
    }
    PredBr->setMetadata(LLVMContext::MD_prof,
                        MDBuilder(PredBr->getParent()->getContext())
                            .createBranchWeights(Weights));
  }
}



