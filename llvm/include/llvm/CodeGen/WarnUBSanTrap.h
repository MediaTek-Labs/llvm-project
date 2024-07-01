//===---------------------------- WarnUBSanTrap.h -------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_WARNUBSANTRAP_H
#define LLVM_CODEGEN_WARNUBSANTRAP_H

#include "llvm/IR/PassManager.h"
#include "llvm/IR/Value.h"

namespace llvm {

class WarnUBSanTrapPass : public PassInfoMixin<WarnUBSanTrapPass> {
  void WarnUnguardedTrapInstr(Function &F, Value *);

public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
  static bool isRequired() { return true; }
};

} // end namespace llvm

#endif // LLVM_CODEGEN_WARNUBSANTRAP_H
