//===---------------------- CodeGen/WarnUBSanTrap.cpp ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This pass looks for calls to llvm::ubsantrap_unique intrinsic function,
// which is a variant of llvm::ubsantrap that preserves source location of
// the trap triggering instruction.
// For each unique trap, it issues a warning and replaces that trap with a
// standard  one
//
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/WarnUBSanTrap.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
using namespace llvm;

void WarnUBSanTrapPass::WarnUnguardedTrapInstr(Function &F, Value *DILocOp) {
  auto *DILoc = cast<MDNode>(cast<MetadataAsValue>(DILocOp)->getMetadata());
  DiagnosticInfoUnsupported Diag(F, "Operation overflow is not guarded",
                                 DebugLoc(DILoc), DS_Warning);
  DiagnosticPrinterRawOStream DP(errs());
  Diag.print(DP);
  errs() << "\n";
}

PreservedAnalyses WarnUBSanTrapPass::run(Module &M, ModuleAnalysisManager &AM) {
  int Change = false;
  StringRef TrapUniqueName = Intrinsic::getName(Intrinsic::ubsantrap_unique);
  // TODO: in llvm 16 add a parameter to this pass
  // indicating if the warning flag is enabled
  // and skip this check if not
  if (auto *TrapUnique = M.getFunction(TrapUniqueName)) {
    std::vector<CallInst *> callSites;
    for (auto &U : TrapUnique->uses())
      if (auto *CI = dyn_cast<CallInst>(U.getUser()))
        callSites.push_back(CI);
    if (!callSites.empty()) {
      Function *UBSanTrap = Intrinsic::getDeclaration(&M, Intrinsic::ubsantrap);
      for (CallInst *CI : callSites) {
        Function *F = CI->getParent()->getParent();
        Value *TrapOp = CI->getOperand(0);
        CallInst *NewCall = CallInst::Create(UBSanTrap, {TrapOp}, "", CI);
        CI->replaceAllUsesWith(NewCall);
        CI->eraseFromParent();
        WarnUnguardedTrapInstr(*F, CI->getOperand(1));
      }
    }
    TrapUnique->eraseFromParent();
    Change = true;
  }
  return Change ? PreservedAnalyses::none() : PreservedAnalyses::all();
}
