//===--------------------------- NanoMipsTrapOpt.h ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_NANOMIPSTRAPOPT_H
#define LLVM_CODEGEN_NANOMIPSTRAPOPT_H

#include "llvm/IR/Function.h"
#include "llvm/IR/PassManager.h"

namespace llvm {

class NanoMipsTrapOptPass : public PassInfoMixin<NanoMipsTrapOptPass> {
  bool internalizeTrapOfAddSub(Function &F, Value *Cond);

public:
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
  bool IsOptNone;
};

} // end namespace llvm

#endif // LLVM_CODEGEN_NANOMIPSTRAPOPT_H
