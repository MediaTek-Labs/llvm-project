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
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/DiagnosticPrinter.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/CommandLine.h"
namespace llvm {
cl::opt<bool>
    SuppressWarnedTraps("suppress-warned-traps", cl::Hidden,
                        cl::desc("Suppress warned ubsan overflow traps"),
                        cl::init(false));
} // namespace llvm

using namespace llvm;

void WarnUBSanTrapPass::WarnUnguardedTrapInstr(Function &F, Value *DILocOp) {
  auto *DILoc = cast<MDNode>(cast<MetadataAsValue>(DILocOp)->getMetadata());
  DebugLoc DL = DILoc->getNumOperands() != 0 ? DebugLoc(DILoc) : DebugLoc();
  DiagnosticInfoUnsupported Diag(F, "Operation overflow is not guarded",
                                 DL, DS_Warning);
  DiagnosticPrinterRawOStream DP(errs());
  Diag.print(DP);
  errs() << "\n";
}

void WarnUBSanTrapPass::WarnUnconditionalTraps(Module &M,
                                               ModuleAnalysisManager &AM) {
  Function *Trap = M.getFunction(Intrinsic::getName(Intrinsic::ubsantrap));
  if (Trap)
    for (Use &U : Trap->uses()) {
      CallInst *CI = dyn_cast<CallInst>(U.getUser());
      Function *F = CI->getFunction();
      auto &FAM = (AM.getResult<FunctionAnalysisManagerModuleProxy>(M)
                   .getManager());
      auto &PDTR = FAM.getResult<PostDominatorTreeAnalysis>(*F);
      if (PDTR.dominates(CI->getParent(), &(F->getEntryBlock()))) {
        DiagnosticInfoUnsupported Diag(
            *F, "Sanitizer trap will always be triggered",
            CI->getDebugLoc(), DS_Warning);
        DiagnosticPrinterRawOStream DP(errs());
        Diag.print(DP);
      }
  }
}

bool WarnUBSanTrapPass::WarnUnguardedTraps(Module &M, ModuleAnalysisManager &AM) {
  int Change = false;
  StringRef TrapUniqueName = Intrinsic::getName(
      SuppressWarnedTraps ? Intrinsic::ubsantrap_unique_return
                          : Intrinsic::ubsantrap_unique);
  // TODO: in llvm 16 add a parameter to this pass
  // indicating if the warning flag is enabled
  // and skip this check if not
  if (auto *TrapUnique = M.getFunction(TrapUniqueName)) {
    std::vector<CallInst *> callSites;
    for (auto &U : TrapUnique->uses())
      if (auto *CI = dyn_cast<CallInst>(U.getUser()))
        callSites.push_back(CI);
    if (!callSites.empty()) {
      Function *UBSanTrap;
      if (!SuppressWarnedTraps)
        UBSanTrap = Intrinsic::getDeclaration(&M, Intrinsic::ubsantrap);
      for (CallInst *CI : callSites) {
        if (!SuppressWarnedTraps) {
          Value *TrapOp = CI->getOperand(0);
          CallInst *NewCall;
          NewCall = CallInst::Create(UBSanTrap, {TrapOp}, "", CI);
          CI->replaceAllUsesWith(NewCall);
        }
        WarnUnguardedTrapInstr(*(CI->getFunction()), CI->getOperand(1));
        CI->eraseFromParent();
      }
    }
    TrapUnique->eraseFromParent();
    Change = true;
  }
  return Change;
}

PreservedAnalyses WarnUBSanTrapPass::run(Module &M, ModuleAnalysisManager &AM) {
  bool Change = WarnUnguardedTraps(M, AM);
  if (!SuppressWarnedTraps)
    WarnUnconditionalTraps(M, AM);
  return Change ? PreservedAnalyses::none() : PreservedAnalyses::all();
}
