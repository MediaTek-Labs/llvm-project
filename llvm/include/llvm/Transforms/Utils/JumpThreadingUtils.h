//===- JumpThreadingUtils.h - JumpThreading Utils ---------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This files declares utilities useful for all passes that perform
// jumpthreading-like transformation
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UTILS_JUMPTHREADINGUTILS_H
#define LLVM_TRANSFORMS_UTILS_JUMPTHREADINGUTILS_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"

namespace llvm {

class BasicBlock;
class Constant;

namespace jumpthreading {

// These are at global scope so static functions can use them too.
using PredValueInfo = SmallVectorImpl<std::pair<Constant *, BasicBlock *>>;
using PredValueInfoTy = SmallVector<std::pair<Constant *, BasicBlock *>, 8>;

} // end namespace jumpthreading

/// Update branch probability information according to conditional
/// branch probability. This is usually made possible for cloned branches
/// in inline instances by the context specific profile in the caller.
/// For instance,
///
///  [Block PredBB]
///  [Branch PredBr]
///  if (t) {
///     Block A;
///  } else {
///     Block B;
///  }
///
///  [Block BB]
///  cond = PN([true, %A], [..., %B]); // PHI node
///  [Branch CondBr]
///  if (cond) {
///    ...  // P(cond == true) = 1%
///  }
///
///  Here we know that when block A is taken, cond must be true, which means
///      P(cond == true | A) = 1
///
///  Given that P(cond == true) = P(cond == true | A) * P(A) +
///                               P(cond == true | B) * P(B)
///  we get:
///     P(cond == true ) = P(A) + P(cond == true | B) * P(B)
///
///  which gives us:
///     P(A) is less than P(cond == true), i.e.
///     P(t == true) <= P(cond == true)
///
///  In other words, if we know P(cond == true) is unlikely, we know
///  that P(t == true) is also unlikely.
///
void updatePredecessorProfileMetadata(
    const jumpthreading::PredValueInfo &PredValues, BasicBlock *BB);
} // end namespace llvm

#endif // LLVM_TRANSFORMS_UTILS_JUMPTHREADINGUTILS_H
