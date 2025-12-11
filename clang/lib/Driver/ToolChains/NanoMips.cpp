//===-- NanoMips.cpp - NaneMips ToolChain Implementations -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "NanoMips.h"
#include "ToolChains/BareMetal.h"
#include "Arch/Mips.h"
#include "CommonArgs.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "clang/Driver/Options.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/VirtualFileSystem.h"

using namespace clang::driver;
using namespace clang::driver::toolchains;
using namespace clang;
using namespace llvm::opt;
using namespace clang::driver::tools;

NanoMips::NanoMips(const Driver &D, const llvm::Triple &Triple,
           const llvm::opt::ArgList &Args) : BareMetal(D, Triple, Args) {
  Multilibs = getMultilibs();
  SelectedMultilibs = getSelectedMultilibs();

  path_list &Paths = getFilePaths();
  SmallString<128> SysRoot(computeSysRoot());
  const std::string OSLibDir = "lib";
  const std::string MultiarchTriple = Triple.getTriple();

  if (!SelectedMultilibs.empty())
    llvm::sys::path::append(SysRoot, OSLibDir, SelectedMultilibs.back().gccSuffix());
  // noehrtti path takes precedence if building with these options
  if (D.CCCIsCXX() &&
       !Args.hasArg(options::OPT_nostdlib, options::OPT_nodefaultlibs) &&
       Args.hasArg(options::OPT_fno_rtti) &&
       Args.hasArg(options::OPT_fno_exceptions))
    addPathIfExists(D, SysRoot + llvm::sys::path::get_separator() + "noehrtti",
                    Paths);

  addPathIfExists(D, SysRoot, Paths);
}


void NanoMipsLinker::ConstructJob(Compilation &C, const JobAction &JA,
                                  const InputInfo &Output, const InputInfoList &Inputs,
                                  const llvm::opt::ArgList &Args,
                                  const char *LinkingOutput) const {
  const toolchains::NanoMips &ToolChain =
      static_cast<const toolchains::NanoMips &>(getToolChain());
  const Driver &D = ToolChain.getDriver();

  const bool HasCRTBeginEndFiles =
      ToolChain.getTriple().hasEnvironment() ||
      (ToolChain.getTriple().getVendor() != llvm::Triple::MipsTechnologies);

  ArgStringList CmdArgs;

  // Silence warning for "clang -g foo.o -o foo"
  Args.ClaimAllArgs(options::OPT_g_Group);
  // and "clang -emit-llvm foo.o -o foo"
  Args.ClaimAllArgs(options::OPT_emit_llvm);
  // and for "clang -w foo.o -o foo". Other warning options are already
  // handled somewhere else.
  Args.ClaimAllArgs(options::OPT_w);

  if (!D.SysRoot.empty())
    CmdArgs.push_back(Args.MakeArgString("--sysroot=" + D.SysRoot));

  if (Args.hasArg(options::OPT_rdynamic))
    CmdArgs.push_back("-export-dynamic");

  if (Args.hasArg(options::OPT_s))
    CmdArgs.push_back("-s");

  CmdArgs.push_back("--eh-frame-hdr");

  CmdArgs.push_back("-static");

  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  if (!Args.hasArg(options::OPT_nostdlib, options::OPT_nostartfiles)) {
    if (HasCRTBeginEndFiles) {
      std::string P;
      if (ToolChain.GetRuntimeLibType(Args) == ToolChain::RLT_CompilerRT) {
        std::string crtbegin = ToolChain.getCompilerRT(Args, "crtbegin",
                                                       ToolChain::FT_Object);
        if (ToolChain.getVFS().exists(crtbegin))
          P = crtbegin;
      }
      if (P.empty())
        P = ToolChain.GetFilePath("crtbegin.o");
      CmdArgs.push_back(Args.MakeArgString(P));
    }

    // Add crtfastmath.o if available and fast math is enabled.
    ToolChain.addFastMathRuntimeIfAvailable(Args, CmdArgs);
  }

  if (Args.hasFlag(options::OPT_mrelax, options::OPT_mno_relax, true)) {
    CmdArgs.push_back("--relax");
  }

  if (Args.hasFlag(options::OPT_mfix_nmips_hw110880,
                   options::OPT_mno_fix_nmips_hw110880, false)) {
    CmdArgs.push_back("--fix-nmips-hw110880");
  }

  Args.AddAllArgs(CmdArgs, options::OPT_L);
  Args.AddAllArgs(CmdArgs, options::OPT_u);

  ToolChain.AddFilePathLibArgs(Args, CmdArgs);

  if (D.isUsingLTO()) {
    assert(!Inputs.empty() && "Must have at least one input.");
    addLTOOptions(ToolChain, Args, CmdArgs, Output, Inputs[0],
                  D.getLTOMode() == LTOK_Thin);

    // No object emitter on NanoMips yet, use external assembler for LTO.
    StringRef CPUName;
    StringRef ABIName;
    mips::getMipsCPUAndABI(Args, getToolChain().getTriple(), CPUName, ABIName);

    if (!ToolChain.useIntegratedAs()) {
      CmdArgs.push_back(Args.MakeArgString("--plugin-opt=-lto-external-asm="
					   + (getToolChain()
					      .GetProgramPath("as"))));
      CmdArgs.push_back("-plugin-opt=-lto-external-asm-arg=-march");
      std::string Arg = "-plugin-opt=-lto-external-asm-arg=";
      Arg += CPUName.data();
      CmdArgs.push_back(Args.MakeArgString(Arg));
      CmdArgs.push_back("-plugin-opt=-lto-external-asm-arg=-EL");
      CmdArgs.push_back("-plugin-opt=-lto-external-asm-arg=-mlegacyregs");
    } else if (Args.hasArgNoClaim(options::OPT_via_file_asm)) {
      CmdArgs.push_back(Args.MakeArgString("--plugin-opt=save-temps"));
      CmdArgs.push_back(Args.MakeArgString("--plugin-opt=-lto-external-asm="
					   + D.ClangExecutable));
      CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=-cc1as");
      CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=-triple");
      std::string Arg = "-plugin-opt=-lto-external-asm-arg=";
      Arg += "nanomips-elf";
      CmdArgs.push_back(Args.MakeArgString(Arg));
      CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=-filetype");
      Arg = "-plugin-opt=-lto-external-asm-arg=obj";
      CmdArgs.push_back(Args.MakeArgString(Arg));
      CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=-target-cpu");
      Arg = "--plugin-opt=-lto-external-asm-arg=";
      Arg += CPUName.data();
      CmdArgs.push_back(Args.MakeArgString(Arg));
      if (Args.hasFlag(options::OPT_mrelax, options::OPT_mno_relax, true)) {
	CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=-target-feature");
	CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=+relax");
      }
      if (Args.hasFlag(options::OPT_mpcrel, options::OPT_mno_pcrel, true)) {
	CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=-target-feature");
	CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=+pcrel");
      }
      if (Args.hasFlag(options::OPT_msoft_float, options::OPT_mno_soft_float, true)) {
	CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=-target-feature");
	CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=+soft-float");
      }
      if (Args.hasFlag(options::OPT_mfix_nmips_hw110880, options::OPT_mno_fix_nmips_hw110880, false)) {
        CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=-target-feature");
        CmdArgs.push_back("--plugin-opt=-lto-external-asm-arg=+nmips-fix-hw110880");
      }
    }
  }

  if (Args.hasArg(options::OPT_Z_Xlinker__no_demangle))
    CmdArgs.push_back("--no-demangle");

  bool NeedsSanitizerDeps = addSanitizerRuntimes(ToolChain, Args, CmdArgs);
  bool NeedsXRayDeps = addXRayRuntime(ToolChain, Args, CmdArgs);
  addLinkerCompressDebugSectionsOption(ToolChain, Args, CmdArgs);
  AddLinkerInputs(ToolChain, Inputs, Args, CmdArgs, JA);
  // The profile runtime also needs access to system libraries.
  getToolChain().addProfileRTLibs(Args, CmdArgs);

  if (D.CCCIsCXX() &&
      !Args.hasArg(options::OPT_nostdlib, options::OPT_nodefaultlibs)) {
    if (ToolChain.ShouldLinkCXXStdlib(Args)) {
      bool OnlyLibstdcxxStatic = Args.hasArg(options::OPT_static_libstdcxx) &&
                                 !Args.hasArg(options::OPT_static);
      if (OnlyLibstdcxxStatic)
        CmdArgs.push_back("-Bstatic");
      ToolChain.AddCXXStdlibLibArgs(Args, CmdArgs);
      if (ToolChain.GetCXXStdlibType(Args) == ToolChain::CST_Libcxx) {
        CmdArgs.push_back("-lc++abi");
      }
      if (OnlyLibstdcxxStatic)
        CmdArgs.push_back("-Bdynamic");
    }
    CmdArgs.push_back("-lm");
  }
  // Silence warnings when linking C code with a C++ '-stdlib' argument.
  Args.ClaimAllArgs(options::OPT_stdlib_EQ);

  if (!Args.hasArg(options::OPT_nostdlib)) {
    if (!Args.hasArg(options::OPT_nodefaultlibs)) {
      CmdArgs.push_back("--start-group");

      if (NeedsSanitizerDeps)
        linkSanitizerRuntimeDeps(ToolChain, Args, CmdArgs);

      if (NeedsXRayDeps)
        linkXRayRuntimeDeps(ToolChain, Args, CmdArgs);

      bool WantPthread = Args.hasArg(options::OPT_pthread) ||
                         Args.hasArg(options::OPT_pthreads);

      // Use the static OpenMP runtime with -static-openmp
      bool StaticOpenMP = Args.hasArg(options::OPT_static_openmp) &&
                          !Args.hasArg(options::OPT_static);

      // FIXME: Only pass GompNeedsRT = true for platforms with libgomp that
      // require librt. Most modern Linux platforms do, but some may not.
      if (addOpenMPRuntime(C, CmdArgs, ToolChain, Args, StaticOpenMP,
                           JA.isHostOffloading(Action::OFK_OpenMP),
                           /* GompNeedsRT= */ true))
        // OpenMP runtimes implies pthreads when using the GNU toolchain.
        // FIXME: Does this really make sense for all GNU toolchains?
        WantPthread = true;

      AddRunTimeLibs(ToolChain, D, CmdArgs, Args);

      if (WantPthread)
        CmdArgs.push_back("-lpthread");

      if (Args.hasArg(options::OPT_fsplit_stack))
        CmdArgs.push_back("--wrap=pthread_create");

      if (!Args.hasArg(options::OPT_nolibc))
        CmdArgs.push_back("-lc");

      CmdArgs.push_back("--end-group");
    }

    if (!Args.hasArg(options::OPT_nostartfiles)) {
      if (HasCRTBeginEndFiles) {
        std::string P;
        if (ToolChain.GetRuntimeLibType(Args) == ToolChain::RLT_CompilerRT) {
          std::string crtend = ToolChain.getCompilerRT(Args, "crtend",
                                                       ToolChain::FT_Object);
          if (ToolChain.getVFS().exists(crtend))
            P = crtend;
        }
        if (P.empty())
          P = ToolChain.GetFilePath("crtend.o");
        CmdArgs.push_back(Args.MakeArgString(P));
      }
    }
  }

  Args.AddAllArgs(CmdArgs, options::OPT_T);

  const char *Exec = Args.MakeArgString(ToolChain.GetLinkerPath());
  C.addCommand(std::make_unique<Command>(JA, *this,
                                         ResponseFileSupport::AtFileCurCP(),
                                         Exec, CmdArgs, Inputs, Output));
}

// Map compiler-rt to a multilib directory
std::string NanoMips::getCompilerRTPath() const {
  SmallString<128> Path(getDriver().ResourceDir);
  if (!SelectedMultilibs.empty() &&
      getDriver().getVFS().exists(Path + SelectedMultilibs.back().gccSuffix()))
    llvm::sys::path::append(Path, SelectedMultilibs.back().gccSuffix());
  else
    llvm::sys::path::append(Path, "lib");

  return std::string(Path.str());
}

// Set sysroot to bin/../nanomips-elf
std::string NanoMips::computeSysRoot() const {
  if (!getDriver().SysRoot.empty() && !SelectedMultilibs.empty())
    return getDriver().SysRoot + SelectedMultilibs.back().osSuffix();

  SmallString<128> SysRootDir;
  llvm::sys::path::append(SysRootDir, getDriver().Dir, "..",
                          getDriver().getTargetTriple());
  if (!SelectedMultilibs.empty())
    SysRootDir += SelectedMultilibs.back().osSuffix();
  return std::string(SysRootDir);
}

void NanoMips::AddClangCXXStdlibIncludeArgs(const ArgList &DriverArgs,
                                            ArgStringList &CC1Args) const {
  if (DriverArgs.hasArg(options::OPT_nostdinc, options::OPT_nostdlibinc,
                        options::OPT_nostdincxx))
    return;

  std::string SysRoot(computeSysRoot());
  if (SysRoot.empty())
    return;

  SmallString<128> Dir(SysRoot);
  switch (GetCXXStdlibType(DriverArgs)) {
  case ToolChain::CST_Libcxx:
  default:
    // Add generic path if nothing else succeeded so far.
    llvm::sys::path::append(Dir, "include", "c++", "v1");
    addSystemInclude(DriverArgs, CC1Args, Dir.str());
    break;
  case ToolChain::CST_Libstdcxx:
    // We only support libc++ toolchain installation.
    break;
  }
}
