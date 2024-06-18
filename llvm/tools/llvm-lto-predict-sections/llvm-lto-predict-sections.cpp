//===-- llvm-lto-predict-sections: predict symbols and sections for LTO bitcode
//---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This program reports the sections and symbols that may be present in final
// link from included LTO bitcode files.
//
// This is a best-effort attempt to predict the sections. Inherently,
// it can't know whether sections may be removed by the LTO
// process. Mergeable sections can't be accounted for either.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/StringExtras.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

static cl::OptionCategory ReportOptionsCategory("Report Options");
static cl::OptionCategory CompileOptionsCategory("Compile-style Options");

static cl::opt<std::string> InputFilename(cl::Positional,
                                          cl::desc("<input bitcode file>"),
                                          cl::init("-"),
                                          cl::value_desc("filename"),
                                          cl::cat(ReportOptionsCategory)
					  );

static cl::opt<bool>
    FFunctionSections("ffunction-sections", cl::Prefix, cl::init(false),
                      cl::desc("Place each function in its own section"),
                      cl::cat(CompileOptionsCategory));

static cl::opt<bool>
    FDataSections("fdata-sections", cl::Prefix, cl::init(false),
                  cl::desc("Place each data object in its own section"),
                  cl::cat(CompileOptionsCategory));

static cl::opt<bool> SymbolsReport("symbols", cl::Prefix, cl::init(false),
                                   cl::desc("Report symbol names"),
                                   cl::cat(ReportOptionsCategory));

static cl::opt<bool> SectionsReport("sections", cl::Prefix, cl::init(false),
                                    cl::desc("Report section names"),
                                    cl::cat(ReportOptionsCategory));

static cl::opt<bool>
    MapReport("map", cl::Prefix, cl::init(false),
              cl::desc("Report mapping of symbols to sections"),
              cl::cat(ReportOptionsCategory));

static cl::opt<bool>
    DefsOnly("defs-only", cl::Prefix, cl::init(false),
	     cl::desc("Only report defined symbols, not declarations"),
	     cl::cat(ReportOptionsCategory));

static cl::opt<bool>
    ExternOnly("externally-visible", cl::Prefix, cl::init(false),
	       cl::desc("Only report externally visible symbols"),
	       cl::cat(ReportOptionsCategory));


static std::string FunctionSectionName(Function &F) {
  if (F.hasSection()) {
    return F.getSection().str();
  } else {
    if (FFunctionSections) {
      return ".text." + F.getName().str();
    } else {
      return ".text";
    }
  }
}

static std::string GlobalSectionName(GlobalVariable &G) {
  if (G.hasSection()) {
    return G.getSection().str();
  } else {
    std::string Prefix;
    if (G.isConstant()) {
      Prefix = ".rodata";
    } else {
      if (G.hasInitializer() && !G.getInitializer()->isZeroValue()) {
        Prefix = ".data";
      } else {
        Prefix = ".bss";
      }
    }
    if (FDataSections) {
      if (G.getName().str()[0] == '.') {
        return Prefix + G.getName().str();
      } else {
        return Prefix + "." + G.getName().str();
      }
    } else {
      return Prefix;
    }
  }
}

static std::map<std::string, std::string> SymbolsToSectionsMap;
static std::set<std::string> SectionNames;
static std::set<std::string> SymbolNames;

static bool ShouldReport(GlobalObject &G) {
  if (ExternOnly && !G.hasExternalLinkage())
    return false;
  else if (DefsOnly && G.isDeclaration())
    return false;
  return true;
}

int main(int argc, char **argv) {
  LLVMContext Context;
  SMDiagnostic Err;
  cl::HideUnrelatedOptions({&ReportOptionsCategory, &CompileOptionsCategory});
  cl::ParseCommandLineOptions(argc, argv, "LLVM LTO section tool\n");

  std::unique_ptr<Module> M = parseIRFile(InputFilename, Err, Context);

  if (!M) {
    Err.print(argv[0], errs());
    return 1;
  }

  auto RecordSymbol = [](std::string SymbolName, std::string SectionName) {
    if (SectionsReport || MapReport) {
      SectionNames.insert(SectionName);
    }
    if (SymbolsReport || MapReport) {
      SymbolNames.insert(SymbolName);
    }
    if (MapReport) {
      SymbolsToSectionsMap[SymbolName] = SectionName;
    }
  };

  for (GlobalVariable &G : M->globals()) {
    if (ShouldReport(G))
      RecordSymbol(G.getName().str(), GlobalSectionName(G));
  }

  for (Function &F : M->functions()) {
    if (ShouldReport(F))
      RecordSymbol(F.getName().str(), FunctionSectionName(F));
  }

  if (SectionsReport) {
    outs() << SectionNames.size() << " sections:\n";
    for (auto &S : SectionNames) {
      outs() << S << "\n";
    }
  }

  if (SymbolsReport) {
    outs() << SymbolNames.size() << " symbols:\n";
    for (auto &S : SymbolNames) {
      outs() << S << "\n";
    }
  }

  if (MapReport) {
    outs() << SymbolNames.size() << " symbols in " << SectionNames.size()
           << " sections\n";
    for (auto &M : SymbolsToSectionsMap) {
      outs() << M.first << " " << M.second << "\n";
    }
  }

  return 0;
}
