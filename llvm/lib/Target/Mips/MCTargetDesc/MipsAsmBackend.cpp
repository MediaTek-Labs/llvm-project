//===-- MipsAsmBackend.cpp - Mips Asm Backend  ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the MipsAsmBackend class.
//
//===----------------------------------------------------------------------===//
//

#include "MCTargetDesc/MipsAsmBackend.h"
#include "MCTargetDesc/MipsABIInfo.h"
#include "MCTargetDesc/MipsFixupKinds.h"
#include "MCTargetDesc/MipsMCTargetDesc.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/LEB128.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

// Prepare value for the target space for it
static unsigned adjustFixupValue(const MCFixup &Fixup, uint64_t Value,
                                 MCContext &Ctx) {

  unsigned Kind = Fixup.getKind();

  // Add/subtract and shift
  switch (Kind) {
  default:
    return 0;
  case FK_Data_2:
  case Mips::fixup_Mips_LO16:
  case Mips::fixup_Mips_GPREL16:
  case Mips::fixup_Mips_GPOFF_HI:
  case Mips::fixup_Mips_GPOFF_LO:
  case Mips::fixup_Mips_GOT_PAGE:
  case Mips::fixup_Mips_GOT_OFST:
  case Mips::fixup_Mips_GOT_DISP:
  case Mips::fixup_Mips_GOT_LO16:
  case Mips::fixup_Mips_CALL_LO16:
  case Mips::fixup_MICROMIPS_GPOFF_HI:
  case Mips::fixup_MICROMIPS_GPOFF_LO:
  case Mips::fixup_MICROMIPS_LO16:
  case Mips::fixup_MICROMIPS_GOT_PAGE:
  case Mips::fixup_MICROMIPS_GOT_OFST:
  case Mips::fixup_MICROMIPS_GOT_DISP:
  case Mips::fixup_MIPS_PCLO16:
    Value &= 0xffff;
    break;
  case FK_DTPRel_4:
  case FK_DTPRel_8:
  case FK_TPRel_4:
  case FK_TPRel_8:
  case FK_GPRel_4:
  case FK_Data_4:
  case FK_Data_8:
  case Mips::fixup_Mips_SUB:
  case Mips::fixup_MICROMIPS_SUB:
    break;
  case Mips::fixup_Mips_PC16:
    // The displacement is then divided by 4 to give us an 18 bit
    // address range. Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 4;
    // We now check if Value can be encoded as a 16-bit signed immediate.
    if (!isInt<16>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC16 fixup");
      return 0;
    }
    break;
  case Mips::fixup_MIPS_PC19_S2:
  case Mips::fixup_MICROMIPS_PC19_S2:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 4;
    // We now check if Value can be encoded as a 19-bit signed immediate.
    if (!isInt<19>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC19 fixup");
      return 0;
    }
    break;
  case Mips::fixup_Mips_26:
    // So far we are only using this type for jumps.
    // The displacement is then divided by 4 to give us an 28 bit
    // address range.
    Value >>= 2;
    break;
  case Mips::fixup_Mips_HI16:
  case Mips::fixup_Mips_GOT:
  case Mips::fixup_MICROMIPS_GOT16:
  case Mips::fixup_Mips_GOT_HI16:
  case Mips::fixup_Mips_CALL_HI16:
  case Mips::fixup_MICROMIPS_HI16:
  case Mips::fixup_MIPS_PCHI16:
    // Get the 2nd 16-bits. Also add 1 if bit 15 is 1.
    Value = ((Value + 0x8000) >> 16) & 0xffff;
    break;
  case Mips::fixup_Mips_HIGHER:
  case Mips::fixup_MICROMIPS_HIGHER:
    // Get the 3rd 16-bits.
    Value = ((Value + 0x80008000LL) >> 32) & 0xffff;
    break;
  case Mips::fixup_Mips_HIGHEST:
  case Mips::fixup_MICROMIPS_HIGHEST:
    // Get the 4th 16-bits.
    Value = ((Value + 0x800080008000LL) >> 48) & 0xffff;
    break;
  case Mips::fixup_MICROMIPS_26_S1:
    Value >>= 1;
    break;
  case Mips::fixup_MICROMIPS_PC7_S1:
    Value -= 4;
    // Forcing a signed division because Value can be negative.
    Value = (int64_t) Value / 2;
    // We now check if Value can be encoded as a 7-bit signed immediate.
    if (!isInt<7>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC7 fixup");
      return 0;
    }
    break;
  case Mips::fixup_MICROMIPS_PC10_S1:
    Value -= 2;
    // Forcing a signed division because Value can be negative.
    Value = (int64_t) Value / 2;
    // We now check if Value can be encoded as a 10-bit signed immediate.
    if (!isInt<10>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC10 fixup");
      return 0;
    }
    break;
  case Mips::fixup_MICROMIPS_PC16_S1:
    Value -= 4;
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 2;
    // We now check if Value can be encoded as a 16-bit signed immediate.
    if (!isInt<16>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC16 fixup");
      return 0;
    }
    break;
  case Mips::fixup_MIPS_PC18_S3:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 8;
    // We now check if Value can be encoded as a 18-bit signed immediate.
    if (!isInt<18>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC18 fixup");
      return 0;
    }
    break;
  case Mips::fixup_MICROMIPS_PC18_S3:
    // Check alignment.
    if ((Value & 7)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC18 fixup");
    }
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 8;
    // We now check if Value can be encoded as a 18-bit signed immediate.
    if (!isInt<18>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC18 fixup");
      return 0;
    }
    break;
  case Mips::fixup_MIPS_PC21_S2:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t) Value / 4;
    // We now check if Value can be encoded as a 21-bit signed immediate.
    if (!isInt<21>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC21 fixup");
      return 0;
    }
    break;
  case Mips::fixup_MIPS_PC26_S2:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t) Value / 4;
    // We now check if Value can be encoded as a 26-bit signed immediate.
    if (!isInt<26>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC26 fixup");
      return 0;
    }
    break;
  case Mips::fixup_MICROMIPS_PC26_S1:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 2;
    // We now check if Value can be encoded as a 26-bit signed immediate.
    if (!isInt<26>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC26 fixup");
      return 0;
    }
    break;
  case Mips::fixup_MICROMIPS_PC21_S1:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 2;
    // We now check if Value can be encoded as a 21-bit signed immediate.
    if (!isInt<21>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC21 fixup");
      return 0;
    }
    break;
  case Mips::fixup_NANOMIPS_PC25_S1:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 2;
    // We now check if Value can be encoded as a 25-bit signed immediate.
    if (!isInt<25>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC25 fixup");
      return 0;
    }
    break;
  case Mips::fixup_NANOMIPS_PC21_S1:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 2;
    // We now check if Value can be encoded as a 21-bit signed immediate.
    if (!isInt<21>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC21 fixup");
      return 0;
    }
    break;
  case Mips::fixup_NANOMIPS_PC14_S1:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 2;
    // We now check if Value can be encoded as a 14-bit signed immediate.
    if (!isInt<14>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC14 fixup");
      return 0;
    }
    break;
  case Mips::fixup_NANOMIPS_PC11_S1:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 2;
    // We now check if Value can be encoded as a 11-bit signed immediate.
    if (!isInt<11>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC11 fixup");
      return 0;
    }
    break;
  case Mips::fixup_NANOMIPS_PC10_S1:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 2;
    // We now check if Value can be encoded as a 10-bit signed immediate.
    if (!isInt<10>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC10 fixup");
      return 0;
    }
    break;
  case Mips::fixup_NANOMIPS_PC7_S1:
    // Forcing a signed division because Value can be negative.
    Value = (int64_t)Value / 2;
    // We now check if Value can be encoded as a 7-bit signed immediate.
    if (!isInt<7>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC7 fixup");
      return 0;
    }
    break;
  case Mips::fixup_NANOMIPS_PC4_S1:
    // Needs to be encoded as non-zero, per the ISA.
    // Force a value of 0xf (all-ones), if necessary.
    if (Value == 0)
      Value = 0xf;
    else
      Value = Value / 2;
    if (!isUInt<4>(Value)) {
      Ctx.reportError(Fixup.getLoc(), "out of range PC4 fixup");
      return 0;
    }
    break;
  case Mips::fixup_NANOMIPS_HI20:
  case Mips::fixup_NANOMIPS_GPREL_HI20:
  case Mips::fixup_NANOMIPS_PCHI20:
  case Mips::fixup_NANOMIPS_GOTPC_HI20:
    // nanoMIPS uses a 20-bit high part to construct 32-bit values
    Value = (Value >> 12) & 0xfffff;
    break;
  case Mips::fixup_NANOMIPS_LO12:
  case Mips::fixup_NANOMIPS_GPREL_LO12:
  case Mips::fixup_NANOMIPS_GOT_LO12:
    // 12-bit low part of a 32-bit value.
    Value = Value & 0xfff;
    break;
  }

  return Value;
}

std::unique_ptr<MCObjectTargetWriter>
MipsAsmBackend::createObjectTargetWriter() const {
  return createMipsELFObjectWriter(TheTriple, IsN32, ABIVersion);
}

// Little-endian fixup data byte ordering:
//   mips32r2:   a | b | x | x
//   microMIPS/nanoMIPS:  x | x | a | b

static bool needsMMLEByteOrder(unsigned Kind) {
  return Kind != Mips::fixup_MICROMIPS_PC10_S1 &&
         Kind >= Mips::fixup_MICROMIPS_26_S1 &&
         Kind != Mips::fixup_NANOMIPS_PC4_S1 &&
         Kind != Mips::fixup_NANOMIPS_PC7_S1 &&
         Kind != Mips::fixup_NANOMIPS_PC10_S1 &&
         Kind < Mips::LastTargetFixupKind;
}

// Calculate index for microMIPS specific little endian byte order
static unsigned calculateMMLEIndex(unsigned i) {
  assert(i <= 3 && "Index out of range!");

  return (1 - i / 2) * 2 + i % 2;
}

/// ApplyFixup - Apply the \p Value for given \p Fixup into the provided
/// data fragment, at the offset specified by the fixup and following the
/// fixup kind as appropriate.
void MipsAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                const MCValue &Target,
                                MutableArrayRef<char> Data, uint64_t Value,
                                bool IsResolved,
                                const MCSubtargetInfo *STI) const {
  MCFixupKind Kind = Fixup.getKind();
  MCContext &Ctx = Asm.getContext();
  Value = adjustFixupValue(Fixup, Value, Ctx);

  if (!Value)
    return; // Doesn't change encoding.

  // Where do we start in the object
  unsigned Offset = Fixup.getOffset();
  // Number of bytes we need to fixup
  unsigned NumBytes = (getFixupKindInfo(Kind).TargetSize + 7) / 8;
  // Used to point to big endian bytes
  unsigned FullSize;

  switch ((unsigned)Kind) {
  case FK_Data_2:
  case Mips::fixup_Mips_16:
  case Mips::fixup_MICROMIPS_PC10_S1:
  case Mips::fixup_NANOMIPS_PC4_S1:
  case Mips::fixup_NANOMIPS_PC7_S1:
  case Mips::fixup_NANOMIPS_PC10_S1:
    FullSize = 2;
    break;
  case FK_Data_8:
  case Mips::fixup_Mips_64:
    FullSize = 8;
    break;
  case FK_Data_4:
  default:
    FullSize = 4;
    break;
  }

  // Grab current value, if any, from bits.
  uint64_t CurVal = 0;

  bool microMipsLEByteOrder = needsMMLEByteOrder((unsigned) Kind);

  for (unsigned i = 0; i != NumBytes; ++i) {
    unsigned Idx = Endian == llvm::endianness::little
                       ? (microMipsLEByteOrder ? calculateMMLEIndex(i) : i)
                       : (FullSize - 1 - i);
    CurVal |= (uint64_t)((uint8_t)Data[Offset + Idx]) << (i*8);
  }

  uint64_t Mask = ((uint64_t)(-1) >>
                    (64 - getFixupKindInfo(Kind).TargetSize));
  CurVal |= Value & Mask;

  // Write out the fixed up bytes back to the code/data bits.
  for (unsigned i = 0; i != NumBytes; ++i) {
    unsigned Idx = Endian == llvm::endianness::little
                       ? (microMipsLEByteOrder ? calculateMMLEIndex(i) : i)
                       : (FullSize - 1 - i);
    Data[Offset + Idx] = (uint8_t)((CurVal >> (i*8)) & 0xff);
  }
}

std::optional<MCFixupKind> MipsAsmBackend::getFixupKind(StringRef Name) const {
  unsigned Type = llvm::StringSwitch<unsigned>(Name)
                      .Case("BFD_RELOC_NONE", ELF::R_MIPS_NONE)
                      .Case("BFD_RELOC_16", ELF::R_MIPS_16)
                      .Case("BFD_RELOC_32", ELF::R_MIPS_32)
                      .Case("BFD_RELOC_64", ELF::R_MIPS_64)
                      .Default(-1u);
  if (Type != -1u)
    return static_cast<MCFixupKind>(FirstLiteralRelocationKind + Type);

  return StringSwitch<std::optional<MCFixupKind>>(Name)
      .Case("R_MIPS_NONE", FK_NONE)
      .Case("R_MIPS_32", FK_Data_4)
      .Case("R_MIPS_CALL_HI16", (MCFixupKind)Mips::fixup_Mips_CALL_HI16)
      .Case("R_MIPS_CALL_LO16", (MCFixupKind)Mips::fixup_Mips_CALL_LO16)
      .Case("R_MIPS_CALL16", (MCFixupKind)Mips::fixup_Mips_CALL16)
      .Case("R_MIPS_GOT16", (MCFixupKind)Mips::fixup_Mips_GOT)
      .Case("R_MIPS_GOT_PAGE", (MCFixupKind)Mips::fixup_Mips_GOT_PAGE)
      .Case("R_MIPS_GOT_OFST", (MCFixupKind)Mips::fixup_Mips_GOT_OFST)
      .Case("R_MIPS_GOT_DISP", (MCFixupKind)Mips::fixup_Mips_GOT_DISP)
      .Case("R_MIPS_GOT_HI16", (MCFixupKind)Mips::fixup_Mips_GOT_HI16)
      .Case("R_MIPS_GOT_LO16", (MCFixupKind)Mips::fixup_Mips_GOT_LO16)
      .Case("R_MIPS_TLS_GOTTPREL", (MCFixupKind)Mips::fixup_Mips_GOTTPREL)
      .Case("R_MIPS_TLS_DTPREL_HI16", (MCFixupKind)Mips::fixup_Mips_DTPREL_HI)
      .Case("R_MIPS_TLS_DTPREL_LO16", (MCFixupKind)Mips::fixup_Mips_DTPREL_LO)
      .Case("R_MIPS_TLS_GD", (MCFixupKind)Mips::fixup_Mips_TLSGD)
      .Case("R_MIPS_TLS_LDM", (MCFixupKind)Mips::fixup_Mips_TLSLDM)
      .Case("R_MIPS_TLS_TPREL_HI16", (MCFixupKind)Mips::fixup_Mips_TPREL_HI)
      .Case("R_MIPS_TLS_TPREL_LO16", (MCFixupKind)Mips::fixup_Mips_TPREL_LO)
      .Case("R_MICROMIPS_CALL16", (MCFixupKind)Mips::fixup_MICROMIPS_CALL16)
      .Case("R_MICROMIPS_GOT_DISP", (MCFixupKind)Mips::fixup_MICROMIPS_GOT_DISP)
      .Case("R_MICROMIPS_GOT_PAGE", (MCFixupKind)Mips::fixup_MICROMIPS_GOT_PAGE)
      .Case("R_MICROMIPS_GOT_OFST", (MCFixupKind)Mips::fixup_MICROMIPS_GOT_OFST)
      .Case("R_MICROMIPS_GOT16", (MCFixupKind)Mips::fixup_MICROMIPS_GOT16)
      .Case("R_MICROMIPS_TLS_GOTTPREL",
            (MCFixupKind)Mips::fixup_MICROMIPS_GOTTPREL)
      .Case("R_MICROMIPS_TLS_DTPREL_HI16",
            (MCFixupKind)Mips::fixup_MICROMIPS_TLS_DTPREL_HI16)
      .Case("R_MICROMIPS_TLS_DTPREL_LO16",
            (MCFixupKind)Mips::fixup_MICROMIPS_TLS_DTPREL_LO16)
      .Case("R_MICROMIPS_TLS_GD", (MCFixupKind)Mips::fixup_MICROMIPS_TLS_GD)
      .Case("R_MICROMIPS_TLS_LDM", (MCFixupKind)Mips::fixup_MICROMIPS_TLS_LDM)
      .Case("R_MICROMIPS_TLS_TPREL_HI16",
            (MCFixupKind)Mips::fixup_MICROMIPS_TLS_TPREL_HI16)
      .Case("R_MICROMIPS_TLS_TPREL_LO16",
            (MCFixupKind)Mips::fixup_MICROMIPS_TLS_TPREL_LO16)
      .Case("R_MIPS_JALR", (MCFixupKind)Mips::fixup_Mips_JALR)
      .Case("R_MICROMIPS_JALR", (MCFixupKind)Mips::fixup_MICROMIPS_JALR)
      .Case("R_NANOMIPS_ALIGN", (MCFixupKind)Mips::fixup_NANOMIPS_ALIGN)
      .Case("R_NANOMIPS_FILL", (MCFixupKind)Mips::fixup_NANOMIPS_FILL)
      .Case("R_NANOMIPS_MAX", (MCFixupKind)Mips::fixup_NANOMIPS_MAX)
      .Case("R_NANOMIPS_INSN32", (MCFixupKind)Mips::fixup_NANOMIPS_INSN32)
      .Case("R_NANOMIPS_FIXED", (MCFixupKind)Mips::fixup_NANOMIPS_FIXED)
      // FIXME: Unsupported
      // .Case("R_NANOMIPS_RELAX", (MCFixupKind)Mips::fixup_NANOMIPS_RELAX)
      // .Case("R_NANOMIPS_NORELAX", (MCFixupKind)Mips::fixup_NANOMIPS_NORELAX)
      .Case("R_NANOMIPS_SAVERESTORE", (MCFixupKind)Mips::fixup_NANOMIPS_SAVERESTORE)
      .Case("R_NANOMIPS_INSN16", (MCFixupKind)Mips::fixup_NANOMIPS_INSN16)
      .Case("R_NANOMIPS_JALR16", (MCFixupKind)Mips::fixup_NANOMIPS_JALR16)
      .Case("R_NANOMIPS_JALR32", (MCFixupKind)Mips::fixup_NANOMIPS_JALR32)
      .Case("R_NANOMIPS_NOTRAMP", (MCFixupKind)Mips::fixup_NANOMIPS_NOTRAMP)
      .Case("R_NANOMIPS_JUMPTABLE_LOAD", (MCFixupKind)Mips::fixup_NANOMIPS_JUMPTABLE_LOAD)
      .Case("R_NANOMIPS_I32", (MCFixupKind)Mips::fixup_NANOMIPS_I32)
      .Default(MCAsmBackend::getFixupKind(Name));
}

const MCFixupKindInfo &MipsAsmBackend::
getFixupKindInfo(MCFixupKind Kind) const {
  const static MCFixupKindInfo LittleEndianInfos[] = {
    // This table *must* be in same the order of fixup_* kinds in
    // MipsFixupKinds.h.
    //
    // name                    offset  bits  flags
    { "fixup_Mips_16",           0,     16,   0 },
    { "fixup_Mips_32",           0,     32,   0 },
    { "fixup_Mips_REL32",        0,     32,   0 },
    { "fixup_Mips_26",           0,     26,   0 },
    { "fixup_Mips_HI16",         0,     16,   0 },
    { "fixup_Mips_LO16",         0,     16,   0 },
    { "fixup_Mips_GPREL16",      0,     16,   0 },
    { "fixup_Mips_LITERAL",      0,     16,   0 },
    { "fixup_Mips_GOT",          0,     16,   0 },
    { "fixup_Mips_PC16",         0,     16,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_Mips_CALL16",       0,     16,   0 },
    { "fixup_Mips_GPREL32",      0,     32,   0 },
    { "fixup_Mips_SHIFT5",       6,      5,   0 },
    { "fixup_Mips_SHIFT6",       6,      5,   0 },
    { "fixup_Mips_64",           0,     64,   0 },
    { "fixup_Mips_TLSGD",        0,     16,   0 },
    { "fixup_Mips_GOTTPREL",     0,     16,   0 },
    { "fixup_Mips_TPREL_HI",     0,     16,   0 },
    { "fixup_Mips_TPREL_LO",     0,     16,   0 },
    { "fixup_Mips_TLSLDM",       0,     16,   0 },
    { "fixup_Mips_DTPREL_HI",    0,     16,   0 },
    { "fixup_Mips_DTPREL_LO",    0,     16,   0 },
    { "fixup_Mips_Branch_PCRel", 0,     16,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_Mips_GPOFF_HI",     0,     16,   0 },
    { "fixup_MICROMIPS_GPOFF_HI",0,     16,   0 },
    { "fixup_Mips_GPOFF_LO",     0,     16,   0 },
    { "fixup_MICROMIPS_GPOFF_LO",0,     16,   0 },
    { "fixup_Mips_GOT_PAGE",     0,     16,   0 },
    { "fixup_Mips_GOT_OFST",     0,     16,   0 },
    { "fixup_Mips_GOT_DISP",     0,     16,   0 },
    { "fixup_Mips_HIGHER",       0,     16,   0 },
    { "fixup_MICROMIPS_HIGHER",  0,     16,   0 },
    { "fixup_Mips_HIGHEST",      0,     16,   0 },
    { "fixup_MICROMIPS_HIGHEST", 0,     16,   0 },
    { "fixup_Mips_GOT_HI16",     0,     16,   0 },
    { "fixup_Mips_GOT_LO16",     0,     16,   0 },
    { "fixup_Mips_CALL_HI16",    0,     16,   0 },
    { "fixup_Mips_CALL_LO16",    0,     16,   0 },
    { "fixup_Mips_PC18_S3",      0,     18,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PC19_S2",      0,     19,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PC21_S2",      0,     21,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PC26_S2",      0,     26,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PCHI16",       0,     16,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PCLO16",       0,     16,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_26_S1",   0,     26,   0 },
    { "fixup_MICROMIPS_HI16",    0,     16,   0 },
    { "fixup_MICROMIPS_LO16",    0,     16,   0 },
    { "fixup_MICROMIPS_GOT16",   0,     16,   0 },
    { "fixup_MICROMIPS_PC7_S1",  0,      7,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC10_S1", 0,     10,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC16_S1", 0,     16,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC26_S1", 0,     26,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC19_S2", 0,     19,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC18_S3", 0,     18,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC21_S1", 0,     21,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_CALL16",  0,     16,   0 },
    { "fixup_MICROMIPS_GOT_DISP",        0,     16,   0 },
    { "fixup_MICROMIPS_GOT_PAGE",        0,     16,   0 },
    { "fixup_MICROMIPS_GOT_OFST",        0,     16,   0 },
    { "fixup_MICROMIPS_TLS_GD",          0,     16,   0 },
    { "fixup_MICROMIPS_TLS_LDM",         0,     16,   0 },
    { "fixup_MICROMIPS_TLS_DTPREL_HI16", 0,     16,   0 },
    { "fixup_MICROMIPS_TLS_DTPREL_LO16", 0,     16,   0 },
    { "fixup_MICROMIPS_GOTTPREL",        0,     16,   0 },
    { "fixup_MICROMIPS_TLS_TPREL_HI16",  0,     16,   0 },
    { "fixup_MICROMIPS_TLS_TPREL_LO16",  0,     16,   0 },
    { "fixup_Mips_SUB",                  0,     64,   0 },
    { "fixup_MICROMIPS_SUB",             0,     64,   0 },
    { "fixup_Mips_JALR",                 0,     32,   0 },
    { "fixup_MICROMIPS_JALR",            0,     32,   0 },
    { "fixup_NANOMIPS_NONE",            0,     32,   0 },
    { "fixup_NANOMIPS_32",	0,	32,	0 },
    { "fixup_NANOMIPS_64",	0,	64,	0 },
    { "fixup_NANOMIPS_NEG",	0,	32,	0 },
    { "fixup_NANOMIPS_ASHIFTR_1",	0,	32,	0 },
    { "fixup_NANOMIPS_UNSIGNED_8",	0,	8,	0 },
    { "fixup_NANOMIPS_SIGNED_8",	0,	8,	0 },
    { "fixup_NANOMIPS_UNSIGNED_16",	0,	16,	0 },
    { "fixup_NANOMIPS_SIGNED_16",	0,	16,	0 },
    { "fixup_NANOMIPS_RELATIVE",	0,	32,	0 },
    { "fixup_NANOMIPS_GLOBAL",	0,	32,	0 },
    { "fixup_NANOMIPS_JUMP_SLOT",	0,	32,	0 },
    { "fixup_NANOMIPS_IRELATIVE",	0,	32,	0 },
    { "fixup_NANOMIPS_PC25_S1",	0,	25,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_PC21_S1",	0,	21,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_PC14_S1",	0,	14,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_PC11_S1",	0,	11,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_PC10_S1",	0,	10,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_PC7_S1",	0,	7,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_PC4_S1",	0,	4,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_GPREL19_S2",	0,	19,	0 },
    { "fixup_NANOMIPS_GPREL18_S3",	0,	18,	0 },
    { "fixup_NANOMIPS_GPREL18",	0,	18,	0 },
    { "fixup_NANOMIPS_GPREL17_S1",	0,	17,	0 },
    { "fixup_NANOMIPS_GPREL16_S2",	0,	16,	0 },
    { "fixup_NANOMIPS_GPREL7_S2",	0,	7,	0 },
    { "fixup_NANOMIPS_GPREL_HI20",	20,	32,	0 },
    { "fixup_NANOMIPS_PCHI20",	0,	20,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_HI20",	0,	20,	0 },
    { "fixup_NANOMIPS_LO12",	0,	12,	0 },
    { "fixup_NANOMIPS_GPREL_I32",	0,	32,	0 },
    { "fixup_NANOMIPS_PC_I32",	0,	32,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_I32",	0,	32,	0 },
    { "fixup_NANOMIPS_GOT_DISP",	0,	32,	0 },
    { "fixup_NANOMIPS_GOTPC_I32",	0,	32,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_GOTPC_HI20",	0,	32,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_GOT_LO12",	0,	32,	0 },
    { "fixup_NANOMIPS_GOT_CALL",	0,	32,	0 },
    { "fixup_NANOMIPS_GOT_PAGE",	0,	32,	0 },
    { "fixup_NANOMIPS_GOT_OFST",	0,	32,	0 },
    { "fixup_NANOMIPS_LO4_S2",		0,	4,	0 },
    { "fixup_NANOMIPS_RESERVED1",	0,	32,	0 },
    { "fixup_NANOMIPS_GPREL_LO12",	0,	12,	0 },
    { "fixup_NANOMIPS_SCN_DISP",	0,	32,	0 },
    { "fixup_NANOMIPS_COPY",	0,	32,	0 },
    { "fixup_NANOMIPS_ALIGN",	0,	0,	0 },
    { "fixup_NANOMIPS_FILL",	0,	0,	0 },
    { "fixup_NANOMIPS_MAX",	0,	0,	0 },
    { "fixup_NANOMIPS_INSN32",	0,	0,	0 },
    { "fixup_NANOMIPS_FIXED",	0,	0,	0 },
    { "fixup_NANOMIPS_NORELAX",	0,	0,	0 },
    { "fixup_NANOMIPS_RELAX",	0,	0,	0 },
    { "fixup_NANOMIPS_SAVERESTORE",	0,	0,	0 },
    { "fixup_NANOMIPS_INSN16",	0,	0,	0 },
    { "fixup_NANOMIPS_JALR32",	0,	32,	0 },
    { "fixup_NANOMIPS_JALR16",	0,	32,	0 },
    { "fixup_NANOMIPS_JUMPTABLE_LOAD",	0,	32,	MCFixupKindInfo::FKF_IsTarget },
    { "fixup_NANOMIPS_FRAME_REG",	0,	32,	0 },
    { "fixup_NANOMIPS_NOTRAMP",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_DTPMOD",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_DTPREL",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_TPREL",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_GD",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_GD_I32",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_LD",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_LD_I32",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_DTPREL12",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_DTPREL16",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_DTPREL_I32",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_GOTTPREL",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_GOTTPREL_PC_I32",	0,	32,	MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_NANOMIPS_TLS_TPREL12",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_TPREL16",	0,	32,	0 },
    { "fixup_NANOMIPS_TLS_TPREL_I32",	0,	32,	0 },
  };
  static_assert(std::size(LittleEndianInfos) == Mips::NumTargetFixupKinds,
                "Not all MIPS little endian fixup kinds added!");

  const static MCFixupKindInfo BigEndianInfos[] = {
    // This table *must* be in same the order of fixup_* kinds in
    // MipsFixupKinds.h.
    //
    // name                    offset  bits  flags
    { "fixup_Mips_16",          16,     16,   0 },
    { "fixup_Mips_32",           0,     32,   0 },
    { "fixup_Mips_REL32",        0,     32,   0 },
    { "fixup_Mips_26",           6,     26,   0 },
    { "fixup_Mips_HI16",        16,     16,   0 },
    { "fixup_Mips_LO16",        16,     16,   0 },
    { "fixup_Mips_GPREL16",     16,     16,   0 },
    { "fixup_Mips_LITERAL",     16,     16,   0 },
    { "fixup_Mips_GOT",         16,     16,   0 },
    { "fixup_Mips_PC16",        16,     16,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_Mips_CALL16",      16,     16,   0 },
    { "fixup_Mips_GPREL32",      0,     32,   0 },
    { "fixup_Mips_SHIFT5",      21,      5,   0 },
    { "fixup_Mips_SHIFT6",      21,      5,   0 },
    { "fixup_Mips_64",           0,     64,   0 },
    { "fixup_Mips_TLSGD",       16,     16,   0 },
    { "fixup_Mips_GOTTPREL",    16,     16,   0 },
    { "fixup_Mips_TPREL_HI",    16,     16,   0 },
    { "fixup_Mips_TPREL_LO",    16,     16,   0 },
    { "fixup_Mips_TLSLDM",      16,     16,   0 },
    { "fixup_Mips_DTPREL_HI",   16,     16,   0 },
    { "fixup_Mips_DTPREL_LO",   16,     16,   0 },
    { "fixup_Mips_Branch_PCRel",16,     16,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_Mips_GPOFF_HI",    16,     16,   0 },
    { "fixup_MICROMIPS_GPOFF_HI", 16,     16,   0 },
    { "fixup_Mips_GPOFF_LO",    16,     16,   0 },
    { "fixup_MICROMIPS_GPOFF_LO", 16,     16,   0 },
    { "fixup_Mips_GOT_PAGE",    16,     16,   0 },
    { "fixup_Mips_GOT_OFST",    16,     16,   0 },
    { "fixup_Mips_GOT_DISP",    16,     16,   0 },
    { "fixup_Mips_HIGHER",      16,     16,   0 },
    { "fixup_MICROMIPS_HIGHER", 16,     16,   0 },
    { "fixup_Mips_HIGHEST",     16,     16,   0 },
    { "fixup_MICROMIPS_HIGHEST",16,     16,   0 },
    { "fixup_Mips_GOT_HI16",    16,     16,   0 },
    { "fixup_Mips_GOT_LO16",    16,     16,   0 },
    { "fixup_Mips_CALL_HI16",   16,     16,   0 },
    { "fixup_Mips_CALL_LO16",   16,     16,   0 },
    { "fixup_Mips_PC18_S3",     14,     18,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PC19_S2",     13,     19,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PC21_S2",     11,     21,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PC26_S2",      6,     26,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PCHI16",      16,     16,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MIPS_PCLO16",      16,     16,  MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_26_S1",   6,     26,   0 },
    { "fixup_MICROMIPS_HI16",   16,     16,   0 },
    { "fixup_MICROMIPS_LO16",   16,     16,   0 },
    { "fixup_MICROMIPS_GOT16",  16,     16,   0 },
    { "fixup_MICROMIPS_PC7_S1",  9,      7,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC10_S1", 6,     10,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC16_S1",16,     16,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC26_S1", 6,     26,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC19_S2",13,     19,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC18_S3",14,     18,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_PC21_S1",11,     21,   MCFixupKindInfo::FKF_IsPCRel },
    { "fixup_MICROMIPS_CALL16", 16,     16,   0 },
    { "fixup_MICROMIPS_GOT_DISP",        16,     16,   0 },
    { "fixup_MICROMIPS_GOT_PAGE",        16,     16,   0 },
    { "fixup_MICROMIPS_GOT_OFST",        16,     16,   0 },
    { "fixup_MICROMIPS_TLS_GD",          16,     16,   0 },
    { "fixup_MICROMIPS_TLS_LDM",         16,     16,   0 },
    { "fixup_MICROMIPS_TLS_DTPREL_HI16", 16,     16,   0 },
    { "fixup_MICROMIPS_TLS_DTPREL_LO16", 16,     16,   0 },
    { "fixup_MICROMIPS_GOTTPREL",        16,     16,   0 },
    { "fixup_MICROMIPS_TLS_TPREL_HI16",  16,     16,   0 },
    { "fixup_MICROMIPS_TLS_TPREL_LO16",  16,     16,   0 },
    { "fixup_Mips_SUB",                   0,     64,   0 },
    { "fixup_MICROMIPS_SUB",              0,     64,   0 },
    { "fixup_Mips_JALR",                  0,     32,   0 },
    { "fixup_MICROMIPS_JALR",             0,     32,   0 }
  };
  static_assert(std::size(BigEndianInfos) ==
		Mips::FirstNanoMipsFixupKind - FirstTargetFixupKind,
                "Not all MIPS big endian fixup kinds added!");

  if (Kind >= FirstLiteralRelocationKind)
    return MCAsmBackend::getFixupKindInfo(FK_NONE);
  if (Kind < FirstTargetFixupKind)
    return MCAsmBackend::getFixupKindInfo(Kind);

  assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
          "Invalid kind!");

  if (Endian == llvm::endianness::little)
    return LittleEndianInfos[Kind - FirstTargetFixupKind];
  return BigEndianInfos[Kind - FirstTargetFixupKind];
}

/// WriteNopData - Write an (optimal) nop sequence of Count bytes
/// to the given output. If the target cannot generate such a sequence,
/// it should return an error.
///
/// \return - True on success.
bool MipsAsmBackend::writeNopData(raw_ostream &OS, uint64_t Count,
                                  const MCSubtargetInfo *STI) const {
  if (TheTriple.isNanoMips()) {
    const char zero[] = {'\x0'};
    const char nop16[] = {'\x08', '\x90'};
    const char nop32[] = {0, '\x80', 0, '\xc0'};
    // Align a mis-aligned instruction stream by writing a singular 0. This
    // is required when embedding odd-sized data in to the instruction stream.
    if (Count % 2) {
      OS.write(zero, 1);
      Count -= 1;
    }
    // Emit one 16-bit NOP, followed by as many 32-bit NOPs
    // as required for desired alignment.
    if (Count % 4) {
      OS.write(nop16, 2);
      Count -= 2;
    }
    while (Count > 0) {
      OS.write(nop32, 4);
      Count -= 4;
    }
  }
  else {
    // Check for a less than instruction size number of bytes
    // FIXME: 16 bit instructions are not handled yet here.
    // We shouldn't be using a hard coded number for instruction size.

    // If the count is not 4-byte aligned, we must be writing data into the text
    // section (otherwise we have unaligned instructions, and thus have far
    // bigger problems), so just write zeros instead.
    OS.write_zeros(Count);
  }
  return true;
}

bool MipsAsmBackend::shouldForceRelocation(const MCAssembler &Asm,
                                           const MCFixup &Fixup,
                                           const MCValue &Target,
                                           const uint64_t,
                                           const MCSubtargetInfo *STI) {
  if (Fixup.getKind() >= FirstLiteralRelocationKind)
    return true;
  const unsigned FixupKind = Fixup.getKind();
  switch (FixupKind) {
  default:
    return false;
  // All these relocations require special processing
  // at linking time. Delegate this work to a linker.
  case Mips::fixup_Mips_CALL_HI16:
  case Mips::fixup_Mips_CALL_LO16:
  case Mips::fixup_Mips_CALL16:
  case Mips::fixup_Mips_GOT:
  case Mips::fixup_Mips_GOT_PAGE:
  case Mips::fixup_Mips_GOT_OFST:
  case Mips::fixup_Mips_GOT_DISP:
  case Mips::fixup_Mips_GOT_HI16:
  case Mips::fixup_Mips_GOT_LO16:
  case Mips::fixup_Mips_GOTTPREL:
  case Mips::fixup_Mips_DTPREL_HI:
  case Mips::fixup_Mips_DTPREL_LO:
  case Mips::fixup_Mips_TLSGD:
  case Mips::fixup_Mips_TLSLDM:
  case Mips::fixup_Mips_TPREL_HI:
  case Mips::fixup_Mips_TPREL_LO:
  case Mips::fixup_Mips_JALR:
  case Mips::fixup_MICROMIPS_CALL16:
  case Mips::fixup_MICROMIPS_GOT_DISP:
  case Mips::fixup_MICROMIPS_GOT_PAGE:
  case Mips::fixup_MICROMIPS_GOT_OFST:
  case Mips::fixup_MICROMIPS_GOT16:
  case Mips::fixup_MICROMIPS_GOTTPREL:
  case Mips::fixup_MICROMIPS_TLS_DTPREL_HI16:
  case Mips::fixup_MICROMIPS_TLS_DTPREL_LO16:
  case Mips::fixup_MICROMIPS_TLS_GD:
  case Mips::fixup_MICROMIPS_TLS_LDM:
  case Mips::fixup_MICROMIPS_TLS_TPREL_HI16:
  case Mips::fixup_MICROMIPS_TLS_TPREL_LO16:
  case Mips::fixup_MICROMIPS_JALR:
  case Mips::fixup_NANOMIPS_PC_I32:
  case Mips::fixup_NANOMIPS_PC25_S1:
  case Mips::fixup_NANOMIPS_PC21_S1:
  case Mips::fixup_NANOMIPS_PC14_S1:
  case Mips::fixup_NANOMIPS_PC11_S1:
  case Mips::fixup_NANOMIPS_PC10_S1:
  case Mips::fixup_NANOMIPS_PC7_S1:
  case Mips::fixup_NANOMIPS_PC4_S1:
  case Mips::fixup_NANOMIPS_PCHI20:
  case Mips::fixup_NANOMIPS_GOTPC_I32:
  case Mips::fixup_NANOMIPS_GOTPC_HI20:
  case Mips::fixup_NANOMIPS_TLS_GOTTPREL_PC_I32:
  case Mips::fixup_NANOMIPS_COPY:
  case Mips::fixup_NANOMIPS_ALIGN:
  case Mips::fixup_NANOMIPS_FILL:
  case Mips::fixup_NANOMIPS_MAX:
  case Mips::fixup_NANOMIPS_INSN32:
  case Mips::fixup_NANOMIPS_FIXED:
  case Mips::fixup_NANOMIPS_NORELAX:
  case Mips::fixup_NANOMIPS_SAVERESTORE:
  case Mips::fixup_NANOMIPS_INSN16:
  case Mips::fixup_NANOMIPS_JALR32:
  case Mips::fixup_NANOMIPS_JALR16:
  case Mips::fixup_NANOMIPS_NOTRAMP:
  case Mips::fixup_NANOMIPS_ASHIFTR_1:
  case Mips::fixup_NANOMIPS_UNSIGNED_16:
  case Mips::fixup_NANOMIPS_UNSIGNED_8:
  case Mips::fixup_NANOMIPS_SIGNED_16:
  case Mips::fixup_NANOMIPS_SIGNED_8:
  case Mips::fixup_NANOMIPS_I32:
    return true;
  }
}

bool MipsAsmBackend::isMicroMips(const MCSymbol *Sym) const {
  if (const auto *ElfSym = dyn_cast<const MCSymbolELF>(Sym)) {
    if (ElfSym->getOther() & ELF::STO_MIPS_MICROMIPS)
      return true;
  }
  return false;
}

// We need to insert R_NANOMIPS_ALIGN relocation type to indicate the
// position of Nops and the total bytes of the Nops have been inserted
// when linker relaxation enabled.
bool MipsAsmBackend::shouldInsertFixupForCodeAlign(MCAssembler &Asm,
                                     MCAlignFragment &AF) {
  MCContext &Ctx = Asm.getContext();
  // Insert the fixup only when alignment is greater than 2
  // and skip when linker relaxation is disabled.
  if (!Ctx.getTargetTriple().isNanoMips() || AF.getAlignment() <= 2 ||
      !AF.getSubtargetInfo()->hasFeature(Mips::FeatureRelax))
    return false;

  uint64_t PadCount = Asm.computeFragmentSize(AF);
  MCSymbol *AlignConst = Ctx.createTempSymbol();
  AlignConst->setVariableValue(MCConstantExpr::create(llvm::Log2(AF.getAlignment()), Ctx));
  (cast<MCSymbolELF>(*AlignConst)).setSize(MCConstantExpr::create(PadCount, Ctx));
  Asm.registerSymbol(*AlignConst);

  const MCSymbolRefExpr *AlignRef = MCSymbolRefExpr::create(AlignConst, Ctx);
  MCFixup Fixup =
      MCFixup::create(0, AlignRef, MCFixupKind(Mips::fixup_NANOMIPS_ALIGN), SMLoc());
  uint64_t FixedValue = 0;
  MCValue NopBytes = MCValue::get(AlignRef, nullptr, 0, 0);
  Asm.getWriter().recordRelocation(Asm, &AF, Fixup, NopBytes, FixedValue);
  return true;
}

namespace {

class WindowsMipsAsmBackend : public MipsAsmBackend {
public:
  WindowsMipsAsmBackend(const Target &T, const MCRegisterInfo &MRI,
                        const MCSubtargetInfo &STI)
      : MipsAsmBackend(T, MRI, STI.getTargetTriple(), STI.getCPU(), false,
      STI.getFeatureBits()[Mips::Feature64BitTimeT] ? 1 : 0) {}

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override {
    return createMipsWinCOFFObjectWriter();
  }
};

} // end anonymous namespace
bool MipsAsmBackend::evaluateTargetFixup(
    const MCAssembler &Asm, const MCFixup &Fixup,
    const MCFragment *DF, const MCValue &Target, const MCSubtargetInfo *STI, uint64_t &Value,
    bool &WasForced) {
  switch (Fixup.getTargetKind()) {
  default:
    llvm_unreachable("Unexpected fixup kind!");
  case Mips::fixup_NANOMIPS_JUMPTABLE_LOAD:
    // Eat the fixup so it does not get emitted to output
    break;
  }

  return true;
}

bool MipsAsmBackend::relaxDwarfLineAddr(const MCAssembler &Asm,
                                  MCDwarfLineAddrFragment &DF,
                                  bool &WasRelaxed) const {
  if (!TheTriple.isNanoMips())
    return false;

  MCContext &C = Asm.getContext();

  int64_t LineDelta = DF.getLineDelta();
  const MCExpr &AddrDelta = DF.getAddrDelta();
  SmallVectorImpl<char> &Data = DF.getContents();
  SmallVectorImpl<MCFixup> &Fixups = DF.getFixups();
  size_t OldSize = Data.size();

  int64_t Value;

  bool IsAbsolute = AddrDelta.evaluateKnownAbsolute(Value, Asm);
  assert(IsAbsolute && "CFA with invalid expression");
  (void)IsAbsolute;

  Data.clear();
  Fixups.clear();
  raw_svector_ostream OS(Data);

  // INT64_MAX is a signal that this is actually a DW_LNE_end_sequence.
  if (LineDelta != INT64_MAX) {
    OS << uint8_t(dwarf::DW_LNS_advance_line);
    encodeSLEB128(LineDelta, OS);
  }

  unsigned Offset;
  std::pair<unsigned, unsigned> Fixup;

  // According to the DWARF specification, the `DW_LNS_fixed_advance_pc` opcode
  // takes a single unsigned half (unencoded) operand. The maximum encodable
  // value is therefore 65535.  Set a conservative upper bound for relaxation.
  if (Value > 60000) {
    unsigned PtrSize = C.getAsmInfo()->getCodePointerSize();

    OS << uint8_t(dwarf::DW_LNS_extended_op);
    encodeULEB128(PtrSize + 1, OS);

    OS << uint8_t(dwarf::DW_LNE_set_address);
    Offset = OS.tell();
    Fixup = std::make_pair(Mips::fixup_NANOMIPS_NEG,
			   Mips::fixup_NANOMIPS_32);
    OS.write_zeros(PtrSize);
  } else {
    OS << uint8_t(dwarf::DW_LNS_fixed_advance_pc);
    Offset = OS.tell();
    Fixup = {Mips::fixup_NANOMIPS_NEG, Mips::fixup_NANOMIPS_UNSIGNED_16};
    support::endian::write<uint16_t>(OS, 0, llvm::endianness::little);
  }

  const MCBinaryExpr &MBE = cast<MCBinaryExpr>(AddrDelta);
  Fixups.push_back(MCFixup::create(
      Offset, MBE.getRHS(), static_cast<MCFixupKind>(std::get<0>(Fixup))));
  Fixups.push_back(MCFixup::create(
      Offset, MBE.getLHS(), static_cast<MCFixupKind>(std::get<1>(Fixup))));

  if (LineDelta == INT64_MAX) {
    OS << uint8_t(dwarf::DW_LNS_extended_op);
    OS << uint8_t(1);
    OS << uint8_t(dwarf::DW_LNE_end_sequence);
  } else {
    OS << uint8_t(dwarf::DW_LNS_copy);
  }

  WasRelaxed = OldSize != Data.size();
  return true;
}

bool MipsAsmBackend::relaxDwarfCFA(const MCAssembler &Asm,
                             MCDwarfCallFrameFragment &DF,
                             bool &WasRelaxed) const {
  if (!TheTriple.isNanoMips())
    return false;

  const MCExpr &AddrDelta = DF.getAddrDelta();
  SmallVectorImpl<char> &Data = DF.getContents();
  SmallVectorImpl<MCFixup> &Fixups = DF.getFixups();
  size_t OldSize = Data.size();

  int64_t Value;
  bool IsAbsolute = AddrDelta.evaluateKnownAbsolute(Value, Asm);

  assert(IsAbsolute && "CFA with invalid expression");
  (void)IsAbsolute;

  Data.clear();
  Fixups.clear();
  raw_svector_ostream OS(Data);

  assert(Asm.getContext().getAsmInfo()->getMinInstAlignment() == 1 &&
         "expected 1-byte alignment");
  if (Value == 0) {
    WasRelaxed = OldSize != Data.size();
    return true;
  }

  auto AddFixups = [&Fixups, &AddrDelta](unsigned Offset,
                                         std::pair<unsigned, unsigned> Fixup) {
    const MCBinaryExpr &MBE = cast<MCBinaryExpr>(AddrDelta);
    Fixups.push_back(MCFixup::create(
        Offset, MBE.getRHS(), static_cast<MCFixupKind>(std::get<0>(Fixup))));
    Fixups.push_back(MCFixup::create(
        Offset, MBE.getLHS(), static_cast<MCFixupKind>(std::get<1>(Fixup))));
  };

  /* Add extra slop in the range for 8/16 bit offsets to account for
     code expansions during link-time relaxation.  Estimate the impact
     to be 1/8 of the range for no particular reason and hope that it
     will be enough. 32 bit offsets are not expected to overflow.
     FIXME: find a tighter bound or a better way to solve this issue. */
  const unsigned Padded = Value + (Value / 8);
  if (isUInt<8>(Padded)) {
    OS << uint8_t(dwarf::DW_CFA_advance_loc1);
    support::endian::write<uint8_t>(OS, 0, llvm::endianness::little);
    AddFixups(1, {Mips::fixup_NANOMIPS_NEG, Mips::fixup_NANOMIPS_UNSIGNED_8});
  } else if (isUInt<16>(Padded)) {
    OS << uint8_t(dwarf::DW_CFA_advance_loc2);
    support::endian::write<uint16_t>(OS, 0, llvm::endianness::little);
    AddFixups(1, {Mips::fixup_NANOMIPS_NEG, Mips::fixup_NANOMIPS_UNSIGNED_16});
  } else if (isUInt<32>(Value)) {
    OS << uint8_t(dwarf::DW_CFA_advance_loc4);
    support::endian::write<uint32_t>(OS, 0, llvm::endianness::little);
    AddFixups(1, {Mips::fixup_NANOMIPS_NEG, Mips::fixup_NANOMIPS_32});
  } else {
    llvm_unreachable("unsupported CFA encoding");
  }

  WasRelaxed = OldSize != Data.size();
  return true;
}

#include "MipsSubtarget.h"

MCAsmBackend *llvm::createMipsAsmBackend(const Target &T,
                                         const MCSubtargetInfo &STI,
                                         const MCRegisterInfo &MRI,
                                         const MCTargetOptions &Options) {
  const Triple &TheTriple = STI.getTargetTriple();
  if (TheTriple.isOSWindows() && TheTriple.isOSBinFormatCOFF())
    return new WindowsMipsAsmBackend(T, MRI, STI);

  MipsABIInfo ABI = MipsABIInfo::computeTargetABI(STI.getTargetTriple(),
                                                  STI.getCPU(), Options);
  // Increment ABI version to indication that libc will be compiled with
  // 64-bit time_t to avoid Y2K38.
  return new MipsAsmBackend(T, MRI, STI.getTargetTriple(), STI.getCPU(),
                            ABI.IsN32(),
			    STI.getFeatureBits()[Mips::Feature64BitTimeT] ? 1 : 0);
}
