//===-------- MipsELFStreamer.cpp - ELF Object Output ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MipsELFStreamer.h"
#include "MipsOptionRecord.h"
#include "MipsTargetStreamer.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Support/Casting.h"
#include "MCTargetDesc/MipsFixupKinds.h"

using namespace llvm;

MipsELFStreamer::MipsELFStreamer(MCContext &Context,
                                 std::unique_ptr<MCAsmBackend> MAB,
                                 std::unique_ptr<MCObjectWriter> OW,
                                 std::unique_ptr<MCCodeEmitter> Emitter)
    : MCELFStreamer(Context, std::move(MAB), std::move(OW),
                    std::move(Emitter)) {
  RegInfoRecord = new MipsRegInfoRecord(this, Context);
  MipsOptionRecords.push_back(
      std::unique_ptr<MipsRegInfoRecord>(RegInfoRecord));
}

void MipsELFStreamer::emitInstruction(const MCInst &Inst,
                                      const MCSubtargetInfo &STI) {
  MCELFStreamer::emitInstruction(Inst, STI);

  MCContext &Context = getContext();
  const MCRegisterInfo *MCRegInfo = Context.getRegisterInfo();

  for (unsigned OpIndex = 0; OpIndex < Inst.getNumOperands(); ++OpIndex) {
    const MCOperand &Op = Inst.getOperand(OpIndex);

    if (!Op.isReg())
      continue;

    unsigned Reg = Op.getReg();
    RegInfoRecord->SetPhysRegUsed(Reg, MCRegInfo);
  }

  createPendingLabelRelocs();
}

void MipsELFStreamer::emitCFIStartProcImpl(MCDwarfFrameInfo &Frame) {
  Frame.Begin = getContext().createTempSymbol();
  MCELFStreamer::emitLabel(Frame.Begin);
}

MCSymbol *MipsELFStreamer::emitCFILabel() {
  MCSymbol *Label = getContext().createTempSymbol("cfi", true);
  MCELFStreamer::emitLabel(Label);
  return Label;
}

void MipsELFStreamer::emitCFIEndProcImpl(MCDwarfFrameInfo &Frame) {
  Frame.End = getContext().createTempSymbol();
  MCELFStreamer::emitLabel(Frame.End);
}

void MipsELFStreamer::createPendingLabelRelocs() {
  MipsTargetELFStreamer *ELFTargetStreamer =
      static_cast<MipsTargetELFStreamer *>(getTargetStreamer());

  // FIXME: Also mark labels when in MIPS16 mode.
  if (ELFTargetStreamer->isMicroMipsEnabled()) {
    for (auto *L : Labels) {
      auto *Label = cast<MCSymbolELF>(L);
      getAssembler().registerSymbol(*Label);
      Label->setOther(ELF::STO_MIPS_MICROMIPS);
    }
  }

  Labels.clear();
}

void MipsELFStreamer::emitLabel(MCSymbol *Symbol, SMLoc Loc) {
  MCELFStreamer::emitLabel(Symbol);
  Labels.push_back(Symbol);
}

void MipsELFStreamer::SwitchSection(MCSection *Section,
                                    const MCExpr *Subsection) {
  MCELFStreamer::SwitchSection(Section, Subsection);
  Labels.clear();
}

static bool isLabelAshr1(const MCExpr *Value) {
  const auto *MBE = dyn_cast<MCBinaryExpr>(Value);
  if (MBE == nullptr)
    return false;

  MCValue E;
  int64_t Shift = 0;
  int64_t &Shiftr = Shift;
  if (MBE->getOpcode() == MCBinaryExpr::AShr &&
      MBE->getLHS()->evaluateAsRelocatable(E, nullptr, nullptr) &&
      MBE->getRHS()->evaluateAsAbsolute(Shiftr) &&
      Shift == 1)
    return true;
  else
    return false;
}

static bool requiresFixups(MCContext &C, const MCExpr *Value,
			   const MCExpr *&LHS, const MCExpr *&RHS) {
  const auto *MBE = dyn_cast<MCBinaryExpr>(Value);
  if (MBE == nullptr)
    return false;

  if (isLabelAshr1(Value))
    return requiresFixups(C, MBE->getLHS(), LHS, RHS);

  MCValue E;
  if (!Value->evaluateAsRelocatable(E, nullptr, nullptr))
    return false;
  if (E.getSymA() == nullptr || E.getSymB() == nullptr)
    return false;

  const auto &A = E.getSymA()->getSymbol();
  const auto &B = E.getSymB()->getSymbol();

  LHS =
    MCBinaryExpr::create(MCBinaryExpr::Add, MCSymbolRefExpr::create(&A, C),
			 MCConstantExpr::create(E.getConstant(), C), C);
  RHS = E.getSymB();

  return (A.isInSection() ? A.getSection().hasInstructions()
	  : !A.getName().empty()) ||
    (B.isInSection() ? B.getSection().hasInstructions()
     : !B.getName().empty());
}

void MipsELFStreamer::emitValueImpl(const MCExpr *Value, unsigned Size,
				    SMLoc Loc, bool isSigned) {
  const MCExpr *A, *B;
  MipsTargetELFStreamer *ELFTargetStreamer =
    static_cast<MipsTargetELFStreamer *>(getTargetStreamer());
  enum Mips::Fixups reloc;
  MCDataFragment *DF;
  bool Ashr1 = isLabelAshr1(Value);

  if (!ELFTargetStreamer->isNanoMipsEnabled() ||
      !requiresFixups(getContext(), Value, A, B)) {
    MCELFStreamer::emitValueImpl(Value, Size, Loc);
    return;
  }

  DF = getOrCreateDataFragment();
  MCStreamer::emitValueImpl(Value, Size, Loc);
  flushPendingLabels(DF, DF->getContents().size());
  MCDwarfLineEntry::make(this, getCurrentSectionOnly());

  switch (Size) {
  case 1:
    reloc = isSigned ? Mips::fixup_NANOMIPS_SIGNED_8 : Mips::fixup_NANOMIPS_UNSIGNED_8;
    break;
  case 2:
    reloc = isSigned ? Mips::fixup_NANOMIPS_SIGNED_16 : Mips::fixup_NANOMIPS_UNSIGNED_16;
    break;
  case 4:
    reloc = Mips::fixup_NANOMIPS_32;
    break;
  default:
    report_fatal_error("Unhandled value size\n");
    break;
  }

  DF->getFixups().push_back(MCFixup::create(
	DF->getContents().size(), B, MCFixupKind(Mips::fixup_NANOMIPS_NEG), Loc));
  if (Ashr1) {
    // Result of SHIFTR is inherently 32-bit and only needs another relocation
    // for byte or half-word sizes.
    DF->getFixups().push_back(MCFixup::create(
	DF->getContents().size(), A,
	MCFixupKind(Mips::fixup_NANOMIPS_ASHIFTR_1), Loc));
    if (Size < 4)
      DF->getFixups().push_back(MCFixup::create(
	DF->getContents().size(), MCConstantExpr::create(0, getContext()),
	MCFixupKind(reloc), Loc));
  }
  else
    DF->getFixups().push_back(MCFixup::create(
	DF->getContents().size(), A, MCFixupKind(reloc), Loc));
  DF->getContents().resize(DF->getContents().size() + Size, 0);
}

void MipsELFStreamer::emitValueImpl(const MCExpr *Value, unsigned Size, SMLoc Loc) {
  MipsELFStreamer::emitValueImpl(Value, Size, Loc, false);
}

void MipsELFStreamer::emitIntValue(uint64_t Value, unsigned Size) {
  MCELFStreamer::emitIntValue(Value, Size);
  Labels.clear();
}

void MipsELFStreamer::EmitMipsOptionRecords() {
  for (const auto &I : MipsOptionRecords)
    I->EmitMipsOptionRecord();
}

// When relaxation is disabled, emit NOP padding without generating
// an ALIGN relocation or enabling hasEmitNops.
void MipsELFStreamer::emitCodeAlignment(unsigned ByteAlignment,
					unsigned MaxBytesToEmit) {
  MipsTargetELFStreamer *ELFTargetStreamer =
    static_cast<MipsTargetELFStreamer *>(getTargetStreamer());
  unsigned Flags = getAssembler().getELFHeaderEFlags();
  if (!ELFTargetStreamer->isNanoMipsEnabled() || 
      (Flags & ELF::EF_NANOMIPS_LINKRELAX))
    MCObjectStreamer::emitCodeAlignment(ByteAlignment, MaxBytesToEmit);
  else if (ByteAlignment >= 4) {
    // Code alignment of 2 is always guaranteed
    // Emit one 16-bit NOP, followed by as many 32-bit NOPs
    // as required for desired alignment.
    MCObjectStreamer::emitValueToAlignment(4, 0x9008,
					   2, 2);
    MCObjectStreamer::emitValueToAlignment(ByteAlignment, 0xc0008000,
					   4, MaxBytesToEmit);
  }
}

// TODO: this can be acheived with a direct override of
// MCObjectFileInfo::getTextSectionAlignment() after updrade to LLVM17
void MipsELFStreamer::InitSections(bool NoExecStack) {
  MipsTargetELFStreamer *ELFTargetStreamer =
    static_cast<MipsTargetELFStreamer *>(getTargetStreamer());
  // Just call parent method for non-nanoMIPS
  if (!ELFTargetStreamer->isNanoMipsEnabled()) {
    MCELFStreamer::InitSections(NoExecStack);
    return;
  }

  // For nanoMIPS, don't set default alignment to 4 for text sections
  // at initialization.
  MCContext &Ctx = getContext();
  if (NoExecStack)
    SwitchSection(Ctx.getAsmInfo()->getNonexecutableStackSection(Ctx));
  else {
    SwitchSection(Ctx.getObjectFileInfo()->getTextSection());
    MCObjectStreamer::emitCodeAlignment(2);
  }
}

MCELFStreamer *llvm::createMipsELFStreamer(
    MCContext &Context, std::unique_ptr<MCAsmBackend> MAB,
    std::unique_ptr<MCObjectWriter> OW, std::unique_ptr<MCCodeEmitter> Emitter,
    bool RelaxAll) {
  return new MipsELFStreamer(Context, std::move(MAB), std::move(OW),
                             std::move(Emitter));
}
