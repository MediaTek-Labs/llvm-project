#ifndef LLVM_TRANSFORMS_UTILS_CANCELCROSSCUDEBUGINFO_H
#define LLVM_TRANSFORMS_UTILS_CANCELCROSSCUDEBUGINFO_H

#include "llvm/IR/PassManager.h"

namespace llvm {

class CancelCrossCUDebugInfo : public PassInfoMixin<CancelCrossCUDebugInfo> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &MA);
};

} // namespace llvm

#endif