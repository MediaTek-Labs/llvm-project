//===- CancelCrossCUDebugInfo.cpp - Enable cross cu canceling--------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/CancelCrossCUDebugInfo.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Instructions.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include <stack>

using namespace llvm;

bool isFunctionSkipped(Function &F) {
  return F.isDeclaration() || !F.hasExactDefinition();
}

DISubprogram *functionCreateAndRemap(
    DIBuilder &DIB, DICompileUnit *CU, DISubprogram *T,
    std::map<std::pair<StringRef, DICompileUnit *>, DISubprogram *>
        &FunctionNameMap,
    ValueToValueMapTy *VMap, LLVMContext &ctx) {
  DISubprogram *SPInline = nullptr;
  auto mapMDtoMD = [&VMap](MDNode *Old, MDNode *New) {
    // Avoid clobbering an existing mapping.
    (void)(*VMap).MD().try_emplace(Old, New);
  };
  if (FunctionNameMap.find(std::make_pair(T->getName(), CU)) ==
      FunctionNameMap.end()) {
    SPInline = DIB.createFunction(
        T->getFile(), T->getName(), T->getName(), T->getFile(), T->getLine(),
        T->getType(), T->getScopeLine(), T->getFlags(), T->getSPFlags());

    for (DINode *DN : T->getRetainedNodes()) {
      auto *NT = dyn_cast<DILocalVariable>(DN);

      if (NT) {
        DILocalVariable *NewVar;
        if (NT->getArg()) {
          NewVar = DIB.createParameterVariable(
              SPInline, NT->getName(), NT->getArg(), SPInline->getFile(),
              NT->getLine(), NT->getType(),
              /*AlwaysPreserve=*/true, NT->getFlags());
        } else {
          NewVar = DIB.createAutoVariable(
              SPInline, NT->getName(), SPInline->getFile(), NT->getLine(),
              NT->getType(), /*AlwaysPreserve=*/true, NT->getFlags(),
              NT->getAlignInBits());
        }

        mapMDtoMD(NT, NewVar);
      } else if (auto *NL = dyn_cast<DILabel>(DN)) {
        DIB.createLabel(SPInline, NL->getName(), SPInline->getFile(),
                        NL->getLine(), true);
      }
    }

    FunctionNameMap.insert(
        std::make_pair(std::make_pair(T->getName(), CU), SPInline));
    DIB.finalizeSubprogram(SPInline);
  } else {
    SPInline = FunctionNameMap[std::make_pair(T->getName(), CU)];
  }

  return SPInline;
}
// TODO: Add support for global vars and types
PreservedAnalyses CancelCrossCUDebugInfo::run(Module &M,
                                              ModuleAnalysisManager &MA) {

  std::map<std::pair<StringRef, DICompileUnit *>, DISubprogram *>
      FunctionNameMap;
  std::map<DICompileUnit *, ValueToValueMapTy *> RemapValuesMaps;

  for (DICompileUnit *CU : M.debug_compile_units()) {
    RemapValuesMaps[CU] = new ValueToValueMapTy();
  }

  for (Function &F : M) {
    if (isFunctionSkipped(F))
      continue;
    auto OrigSP = F.getSubprogram();
    DICompileUnit *CU = OrigSP->getUnit();

    DIBuilder DIB(M, true, CU);
    LLVMContext &Ctx = M.getContext();

    ValueToValueMapTy *VMap = RemapValuesMaps[CU];

    auto mapMDtoMD = [&VMap](MDNode *Old, MDNode *New) {
      // Avoid clobbering an existing mapping.
      (void)(*VMap).MD().try_emplace(Old, New);
    };

    DISubprogram *SPInline = nullptr;
    for (BasicBlock &BB : F) {
      for (Instruction &I : BB) {
        auto DebugLoc = I.getDebugLoc();
        if (!DebugLoc)
          continue;

        auto InlineLoc = DebugLoc.getInlinedAt();

        if (InlineLoc) {
          auto Scope = DebugLoc.getScope();

          if (auto *T = dyn_cast<DILexicalBlock>(Scope)) {
            std::stack<DILexicalBlock *> LexBlockStack;
            LexBlockStack.push(T);
            if (T->getSubprogram()->getUnit() != CU) {
              while (auto *TT = dyn_cast<DILexicalBlock>(T->getScope())) {
                LexBlockStack.push(TT);
                T = TT;
              }
              SPInline = functionCreateAndRemap(DIB, CU, T->getSubprogram(),
                                                FunctionNameMap, VMap, Ctx);
              assert(SPInline && "Error when remaping or creating a function");
              DILexicalBlock *prev = LexBlockStack.top();
              LexBlockStack.pop();
              if (VMap->MD().find(prev) == VMap->MD().end()) {
                DILexicalBlock *Temp =
                    DIB.createLexicalBlock(SPInline, prev->getFile(),
                                           prev->getLine(), prev->getColumn());
                mapMDtoMD(prev, Temp);
                prev = Temp;
              } else {
                prev = dyn_cast<DILexicalBlock>(
                    VMap->MD().find(prev)->getSecond().get());
                assert(prev && "Invalid cast when updating DILexicalBlocks");
              }

              while (!LexBlockStack.empty()) {
                DILexicalBlock *CurLexBlock = LexBlockStack.top();
                LexBlockStack.pop();

                if (VMap->MD().find(CurLexBlock) == VMap->MD().end()) {
                  DILexicalBlock *Temp = DIB.createLexicalBlock(
                      prev, CurLexBlock->getFile(), CurLexBlock->getLine(),
                      CurLexBlock->getColumn());
                  mapMDtoMD(CurLexBlock, Temp);
                  prev = Temp;
                } else {
                  prev = dyn_cast<DILexicalBlock>(
                      VMap->MD().find(CurLexBlock)->getSecond().get());
                  assert(prev && "Invalid cast when updating DILexicalBlocks");
                }
              }
              I.setDebugLoc(DILocation::get(
                  Ctx, DebugLoc.getLine(), DebugLoc.getCol(), prev,
                  DebugLoc.getInlinedAt(), DebugLoc.isImplicitCode()));
              RemapInstruction(&I, *VMap,
                               RF_IgnoreMissingLocals |
                                   RF_ReuseAndMutateDistinctMDs);
            }
          } else if (auto *T = dyn_cast<DISubprogram>(Scope)) {
            if (T->getUnit() != CU) {

              SPInline = functionCreateAndRemap(DIB, CU, T, FunctionNameMap,
                                                VMap, Ctx);
              assert(SPInline && "Error when remaping or creating a function");
              I.setDebugLoc(DILocation::get(
                  Ctx, DebugLoc.getLine(), DebugLoc.getCol(), SPInline,
                  DebugLoc.getInlinedAt(), DebugLoc.isImplicitCode()));
              RemapInstruction(&I, *VMap,
                               RF_IgnoreMissingLocals |
                                   RF_ReuseAndMutateDistinctMDs);
            }
          }
        }
      }
    }

    DIB.finalize();
  }

  for (DICompileUnit *CU : M.debug_compile_units()) {
    ValueToValueMapTy *temp = RemapValuesMaps[CU];
    RemapValuesMaps[CU] = nullptr;
    delete temp;
  }

  return PreservedAnalyses::all();
}

namespace {
class CancelCrossCUDebugInfoLegacyPass : public ModulePass {
public:
  static char ID;
  CancelCrossCUDebugInfoLegacyPass() : ModulePass(ID) {
    initializeDAEPass(*PassRegistry::getPassRegistry());
  }

  bool runOnModule(Module &M) override {
    if (skipModule(M))
      return false;
    CancelCrossCUDebugInfo USFEP = CancelCrossCUDebugInfo();
    ModuleAnalysisManager DummyMAM;
    PreservedAnalyses PA = USFEP.run(M, DummyMAM);
    return !PA.areAllPreserved();
  }
};
} // end of anonymous namespace

char CancelCrossCUDebugInfoLegacyPass::ID = 0;

INITIALIZE_PASS_BEGIN(CancelCrossCUDebugInfoLegacyPass,
                      "cancel-cross-cu-debug-info", "Cancel CrossCU Debug Info",
                      false, false)
INITIALIZE_PASS_END(CancelCrossCUDebugInfoLegacyPass,
                    "cancel-cross-cu-debug-info", "Cancel CrossCU Debug Info",
                    false, false)

ModulePass *llvm::createCancelCrossCUDebugInfoLegacyPass() {
  return new CancelCrossCUDebugInfoLegacyPass();
}
