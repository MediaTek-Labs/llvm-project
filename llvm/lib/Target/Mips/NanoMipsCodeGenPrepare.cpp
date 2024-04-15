//===----- NanoMipsCodeGenPrepare.cpp ----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This is a NanoMips specific version of CodeGenPrepare.
// It munges the code in the input function to better prepare it for
// SelectionDAG-based code generation. This works around limitations in it's
// basic-block-at-a-time approach.
//
//===----------------------------------------------------------------------===//

#include "Mips.h"
#include "MipsTargetMachine.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicsMips.h"
#include "llvm/IR/Instructions.h"


using namespace llvm;

#define DEBUG_TYPE "nmips-codegenprepare"
#define PASS_NAME "NanoMips CodeGenPrepare"

static cl::opt<bool> DisableCodeGenPrepare(
  "disable-nmips-codegen-prepare", cl::Hidden, cl::init(false),
  cl::desc("Disable nanoMIPS CodeGenPrepare"));

static cl::opt<bool> DisableCondtrap(
  "disable-nmips-condtrap", cl::Hidden, cl::init(true),
  cl::desc("Disable nanoMIPS CodeGenPrepare"));

extern cl::opt<int> NMDebugTrapCode;

extern cl::opt<int> NMUBSanTrapCode;

STATISTIC(NumGEPSplit, "Number of GEP instructions split");

namespace {

class NanoMipsCodeGenPrepare : public FunctionPass,
                               public InstVisitor<NanoMipsCodeGenPrepare, bool> {
  DenseMap<GlobalValue*, Instruction*> GVMap;

public:
  static char ID;

  NanoMipsCodeGenPrepare() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override;

  StringRef getPassName() const override { return PASS_NAME; }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    //TODO(draganm) Make this conditional on ubsan trap presence
    if (DisableCondtrap) {
      AU.setPreservesCFG();
    }
  }

  bool visitInstruction(Instruction &I) { return false; }
  bool visitGetElementPtrInst(GetElementPtrInst &GEP);
  bool visitLoadInst(LoadInst &Load);
  bool visitStoreInst(StoreInst &Store);
  bool visitLoadStoreinst(Instruction &LoadStore, int PointerOperadIndex);
};

} // end anonymous namespace

bool NanoMipsCodeGenPrepare::visitLoadStoreinst(Instruction &LoadStore,
                                                int PointerOperadIndex) {
  Value* P = LoadStore.getOperand(PointerOperadIndex);
  if (ConstantExpr *E = dyn_cast<ConstantExpr>(P)) {
    if (GEPOperator *GEP = dyn_cast<GEPOperator>(E)) {
      SmallVector<Value*, 4> IdxList(GEP->indices());
      GetElementPtrInst *NewGEP = GetElementPtrInst::Create(
                                    GEP->getSourceElementType(), 
                                    GEP->getPointerOperand(), 
                                    IdxList, 
                                    "gep", 
                                    &LoadStore);
      LoadStore.setOperand(PointerOperadIndex, NewGEP);
      visitGetElementPtrInst(*NewGEP);
      return true;
    }
  }
  return false;
}

bool NanoMipsCodeGenPrepare::visitGetElementPtrInst(GetElementPtrInst &GEP) {
  if (GlobalValue *GV = dyn_cast<GlobalValue>(GEP.getPointerOperand())) {
    if (!GEP.hasAllConstantIndices())
      return false;
    Instruction* NewBase;
    DenseMap<GlobalValue*, Instruction*>::iterator Found;
    if ((Found = GVMap.find(GV)) != GVMap.end()) {
      NewBase = Found->second;
    } else {
     IRBuilder<> IRB(&GEP);

     Module *M = IRB.GetInsertBlock()->getParent()->getParent();
     NewBase = IRB.CreateCall(Intrinsic::getDeclaration(M, Intrinsic::mips_hide, {GV->getType()}), {GV});

     GVMap.insert({GV, NewBase});
    }
    GEP.setOperand(0, NewBase);
    NumGEPSplit++;
    return true;
  }
  return false;
}


bool NanoMipsCodeGenPrepare::visitLoadInst(LoadInst &Load) {
  return visitLoadStoreinst(Load, Load.getPointerOperandIndex());
}
bool NanoMipsCodeGenPrepare::visitStoreInst(StoreInst &Store) {
  return visitLoadStoreinst(Store, Store.getPointerOperandIndex());
}

bool NanoMipsCodeGenPrepare::runOnFunction(Function &F) {
  if (skipFunction(F))
    return false;

  if (DisableCodeGenPrepare)
    return false;

  bool MadeChange = false;

  if (!DisableCondtrap) {
    SmallVector<IntrinsicInst *, 4> Traps;
    for (auto &BB : F) {
      if (auto *II = dyn_cast<IntrinsicInst>(BB.getFirstNonPHIOrDbg())) {
        Intrinsic::ID ID = II->getIntrinsicID();
        if (ID == Intrinsic::ubsantrap || ID == Intrinsic::debugtrap) {
          Traps.push_back(II);
        }
      }
    }

    for (auto TI : Traps) {
      for (auto BB :
           llvm::make_early_inc_range(predecessors(TI->getParent()))) {
        BranchInst *BI = dyn_cast<BranchInst>(BB->getTerminator());
        if (!BI || !BI->isConditional())
          continue;

        bool IsTrue = BI->getSuccessor(0) == TI->getParent();
        TI->getParent()->removePredecessor(BB);

        auto Cond = BI->getCondition();

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
        CallInst::Create(
            Intrinsic::getDeclaration(F.getParent(), Intrinsic::mips_condtrap),
            Args, "", BI);
        BranchInst::Create(BI->getSuccessor(IsTrue), BI);
        BI->eraseFromParent();
        MadeChange = true;
      }
    }
  }

  for (auto &BB : F) {
    GVMap.clear();
    for (Instruction &I : llvm::make_early_inc_range(BB))
      MadeChange |= visit(I);
  }

  return MadeChange;
}

INITIALIZE_PASS_BEGIN(NanoMipsCodeGenPrepare, DEBUG_TYPE, PASS_NAME, false, false)
INITIALIZE_PASS_END(NanoMipsCodeGenPrepare, DEBUG_TYPE, PASS_NAME, false, false)

char NanoMipsCodeGenPrepare::ID = 0;

FunctionPass *llvm::createNanoMipsCodeGenPreparePass() {
  return new NanoMipsCodeGenPrepare();
}
