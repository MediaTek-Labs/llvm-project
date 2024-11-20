//===--------- NanoMipsDspPeephole.cpp - nanoMIPS peephole pass ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file This file contains a pass that performs DSP related peephole
/// optimizations. This pass should be run before register allocation.
//
//===----------------------------------------------------------------------===//

#include "Mips.h"
#include "MipsSubtarget.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"

using namespace llvm;

#define PASS_NAME "nanoMIPS DSP Peephole pass"
#define DEBUG_TYPE "nm-dsp-peep"

namespace {
struct NMDspPeephole : public MachineFunctionPass {
  using InstrList = SmallVector<MachineInstr *, 11>;
  using MBBIter = MachineBasicBlock::iterator;
  static char ID;
  const MipsSubtarget *STI;
  const TargetRegisterInfo *TRI;
  const MachineRegisterInfo *MRI;
  NMDspPeephole() : MachineFunctionPass(ID) {}
  StringRef getPassName() const override { return PASS_NAME; }
  bool removeRedundantMoveFromAcc(MachineBasicBlock &);
  MachineInstr *getMIDefiningReg(Register, unsigned);
  Register firstAccOperand(MachineInstr &MI);
  bool runOnMachineFunction(MachineFunction &Fn) override;
};
} // namespace

char NMDspPeephole::ID = 0;

MachineInstr *NMDspPeephole::getMIDefiningReg(Register Reg, unsigned Opcode) {
  if (Reg.isVirtual()) {
    MachineInstr *DefMI = MRI->getVRegDef(Reg);
    if (DefMI && DefMI->getOpcode() == Opcode)
      return DefMI;
  }
  return nullptr;
}

Register NMDspPeephole::firstAccOperand(MachineInstr &MI) {
  for (MachineOperand &MO : MI.operands()) {
    if (MO.isDef())
      continue;
    if (MO.isReg()) {
      Register Reg = MO.getReg();
      const TargetRegisterClass *VirtRegClass = MRI->getRegClass(Reg);
      if (VirtRegClass == &Mips::ACC64DSPNMRegClass) {
        return Reg;
      }
    }
  }
  return Mips::NoRegister;
}

bool NMDspPeephole::removeRedundantMoveFromAcc(MachineBasicBlock &MBB) {
  bool Modified = false;
  for (MachineBasicBlock::reverse_iterator I = MBB.rbegin(), E = MBB.rend();
       I != E; ++I) {
    MachineInstr &MI = *I;
    Register Acc = firstAccOperand(MI);
    if (Acc != Mips::NoRegister) {
      if (auto *MTLOHI = getMIDefiningReg(Acc, Mips::PseudoMTLOHI_DSP_NM)) {
        auto OpLo = MTLOHI->getOperand(1).getReg();
        auto OpHi = MTLOHI->getOperand(2).getReg();
        MachineInstr *DefLo, *DefHi;
        if ((DefLo = getMIDefiningReg(OpLo, Mips::MFLO_NM)) &&
            MRI->hasOneNonDBGUse(OpLo) &&
            (DefHi = getMIDefiningReg(OpHi, Mips::MFHI_NM)) &&
            MRI->hasOneNonDBGUse(OpHi)) {
          if (DefLo->getOperand(1).getReg() == DefHi->getOperand(1).getReg()) {
            MI.substituteRegister(Acc, DefHi->getOperand(1).getReg(), 0, *TRI);
            MTLOHI->eraseFromParent();
            DefLo->eraseFromParent();
            DefHi->eraseFromParent();
            Modified = true;
          }
        }
      }
    }
  }
  return Modified;
}

bool NMDspPeephole::runOnMachineFunction(MachineFunction &Fn) {
  STI = &static_cast<const MipsSubtarget &>(Fn.getSubtarget());
  TRI = STI->getRegisterInfo();
  MRI = &Fn.getRegInfo();
  bool Modified = false;
  for (MachineFunction::iterator MFI = Fn.begin(), E = Fn.end(); MFI != E;
       ++MFI) {
    MachineBasicBlock &MBB = *MFI;
    Modified |= removeRedundantMoveFromAcc(MBB);
  }
  return Modified;
}

INITIALIZE_PASS(NMDspPeephole, DEBUG_TYPE, PASS_NAME, false, false)

namespace llvm {
FunctionPass *createNanoMipsDspPeepholePass() { return new NMDspPeephole(); }
} // namespace llvm
