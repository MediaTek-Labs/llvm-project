//===-- MipsFixupKinds.h - Mips Specific Fixup Entries ----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MIPS_MCTARGETDESC_MIPSFIXUPKINDS_H
#define LLVM_LIB_TARGET_MIPS_MCTARGETDESC_MIPSFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace Mips {
  // Although most of the current fixup types reflect a unique relocation
  // one can have multiple fixup types for a given relocation and thus need
  // to be uniquely named.
  //
  // This table *must* be in the same order of
  // MCFixupKindInfo Infos[Mips::NumTargetFixupKinds]
  // in MipsAsmBackend.cpp.
  //
  enum Fixups {
    // Branch fixups resulting in R_MIPS_16.
    fixup_Mips_16 = FirstTargetFixupKind,

    // Pure 32 bit data fixup resulting in - R_MIPS_32.
    fixup_Mips_32,

    // Full 32 bit data relative data fixup resulting in - R_MIPS_REL32.
    fixup_Mips_REL32,

    // Jump 26 bit fixup resulting in - R_MIPS_26.
    fixup_Mips_26,

    // Pure upper 16 bit fixup resulting in - R_MIPS_HI16.
    fixup_Mips_HI16,

    // Pure lower 16 bit fixup resulting in - R_MIPS_LO16.
    fixup_Mips_LO16,

    // 16 bit fixup for GP offest resulting in - R_MIPS_GPREL16.
    fixup_Mips_GPREL16,

    // 16 bit literal fixup resulting in - R_MIPS_LITERAL.
    fixup_Mips_LITERAL,

    // Symbol fixup resulting in - R_MIPS_GOT16.
    fixup_Mips_GOT,

    // PC relative branch fixup resulting in - R_MIPS_PC16.
    fixup_Mips_PC16,

    // resulting in - R_MIPS_CALL16.
    fixup_Mips_CALL16,

    // resulting in - R_MIPS_GPREL32.
    fixup_Mips_GPREL32,

    // resulting in - R_MIPS_SHIFT5.
    fixup_Mips_SHIFT5,

    // resulting in - R_MIPS_SHIFT6.
    fixup_Mips_SHIFT6,

    // Pure 64 bit data fixup resulting in - R_MIPS_64.
    fixup_Mips_64,

    // resulting in - R_MIPS_TLS_GD.
    fixup_Mips_TLSGD,

    // resulting in - R_MIPS_TLS_GOTTPREL.
    fixup_Mips_GOTTPREL,

    // resulting in - R_MIPS_TLS_TPREL_HI16.
    fixup_Mips_TPREL_HI,

    // resulting in - R_MIPS_TLS_TPREL_LO16.
    fixup_Mips_TPREL_LO,

    // resulting in - R_MIPS_TLS_LDM.
    fixup_Mips_TLSLDM,

    // resulting in - R_MIPS_TLS_DTPREL_HI16.
    fixup_Mips_DTPREL_HI,

    // resulting in - R_MIPS_TLS_DTPREL_LO16.
    fixup_Mips_DTPREL_LO,

    // PC relative branch fixup resulting in - R_MIPS_PC16
    fixup_Mips_Branch_PCRel,

    // resulting in - R_MIPS_GPREL16/R_MIPS_SUB/R_MIPS_HI16
    //                R_MICROMIPS_GPREL16/R_MICROMIPS_SUB/R_MICROMIPS_HI16
    fixup_Mips_GPOFF_HI,
    fixup_MICROMIPS_GPOFF_HI,

    // resulting in - R_MIPS_GPREL16/R_MIPS_SUB/R_MIPS_LO16
    //                R_MICROMIPS_GPREL16/R_MICROMIPS_SUB/R_MICROMIPS_LO16
    fixup_Mips_GPOFF_LO,
    fixup_MICROMIPS_GPOFF_LO,

    // resulting in - R_MIPS_PAGE
    fixup_Mips_GOT_PAGE,

    // resulting in - R_MIPS_GOT_OFST
    fixup_Mips_GOT_OFST,

    // resulting in - R_MIPS_GOT_DISP
    fixup_Mips_GOT_DISP,

    // resulting in - R_MIPS_HIGHER/R_MICROMIPS_HIGHER
    fixup_Mips_HIGHER,
    fixup_MICROMIPS_HIGHER,

    // resulting in - R_MIPS_HIGHEST/R_MICROMIPS_HIGHEST
    fixup_Mips_HIGHEST,
    fixup_MICROMIPS_HIGHEST,

    // resulting in - R_MIPS_GOT_HI16
    fixup_Mips_GOT_HI16,

    // resulting in - R_MIPS_GOT_LO16
    fixup_Mips_GOT_LO16,

    // resulting in - R_MIPS_CALL_HI16
    fixup_Mips_CALL_HI16,

    // resulting in - R_MIPS_CALL_LO16
    fixup_Mips_CALL_LO16,

    // resulting in - R_MIPS_PC18_S3
    fixup_MIPS_PC18_S3,

    // resulting in - R_MIPS_PC19_S2
    fixup_MIPS_PC19_S2,

    // resulting in - R_MIPS_PC21_S2
    fixup_MIPS_PC21_S2,

    // resulting in - R_MIPS_PC26_S2
    fixup_MIPS_PC26_S2,

    // resulting in - R_MIPS_PCHI16
    fixup_MIPS_PCHI16,

    // resulting in - R_MIPS_PCLO16
    fixup_MIPS_PCLO16,

    // resulting in - R_MICROMIPS_26_S1
    fixup_MICROMIPS_26_S1,

    // resulting in - R_MICROMIPS_HI16
    fixup_MICROMIPS_HI16,

    // resulting in - R_MICROMIPS_LO16
    fixup_MICROMIPS_LO16,

    // resulting in - R_MICROMIPS_GOT16
    fixup_MICROMIPS_GOT16,

    // resulting in - R_MICROMIPS_PC7_S1
    fixup_MICROMIPS_PC7_S1,

    // resulting in - R_MICROMIPS_PC10_S1
    fixup_MICROMIPS_PC10_S1,

    // resulting in - R_MICROMIPS_PC16_S1
    fixup_MICROMIPS_PC16_S1,

    // resulting in - R_MICROMIPS_PC26_S1
    fixup_MICROMIPS_PC26_S1,

    // resulting in - R_MICROMIPS_PC19_S2
    fixup_MICROMIPS_PC19_S2,

    // resulting in - R_MICROMIPS_PC18_S3
    fixup_MICROMIPS_PC18_S3,

    // resulting in - R_MICROMIPS_PC21_S1
    fixup_MICROMIPS_PC21_S1,

    // resulting in - R_MICROMIPS_CALL16
    fixup_MICROMIPS_CALL16,

    // resulting in - R_MICROMIPS_GOT_DISP
    fixup_MICROMIPS_GOT_DISP,

    // resulting in - R_MICROMIPS_GOT_PAGE
    fixup_MICROMIPS_GOT_PAGE,

    // resulting in - R_MICROMIPS_GOT_OFST
    fixup_MICROMIPS_GOT_OFST,

    // resulting in - R_MICROMIPS_TLS_GD
    fixup_MICROMIPS_TLS_GD,

    // resulting in - R_MICROMIPS_TLS_LDM
    fixup_MICROMIPS_TLS_LDM,

    // resulting in - R_MICROMIPS_TLS_DTPREL_HI16
    fixup_MICROMIPS_TLS_DTPREL_HI16,

    // resulting in - R_MICROMIPS_TLS_DTPREL_LO16
    fixup_MICROMIPS_TLS_DTPREL_LO16,

    // resulting in - R_MICROMIPS_TLS_GOTTPREL.
    fixup_MICROMIPS_GOTTPREL,

    // resulting in - R_MICROMIPS_TLS_TPREL_HI16
    fixup_MICROMIPS_TLS_TPREL_HI16,

    // resulting in - R_MICROMIPS_TLS_TPREL_LO16
    fixup_MICROMIPS_TLS_TPREL_LO16,

    // resulting in - R_MIPS_SUB/R_MICROMIPS_SUB
    fixup_Mips_SUB,
    fixup_MICROMIPS_SUB,

    // resulting in - R_MIPS_JALR/R_MICROMIPS_JALR
    fixup_Mips_JALR,
    fixup_MICROMIPS_JALR,
    LastMipsFixupKind = fixup_MICROMIPS_JALR,

    // NanoMIPS relocations
    FirstNanoMipsFixupKind,
    fixup_NANOMIPS_NONE = FirstNanoMipsFixupKind,
    fixup_NANOMIPS_32,
    fixup_NANOMIPS_64,
    fixup_NANOMIPS_NEG,
    fixup_NANOMIPS_ASHIFTR_1,
    fixup_NANOMIPS_UNSIGNED_8,
    fixup_NANOMIPS_SIGNED_8,
    fixup_NANOMIPS_UNSIGNED_16,
    fixup_NANOMIPS_SIGNED_16,
    fixup_NANOMIPS_RELATIVE,
    fixup_NANOMIPS_GLOBAL,
    fixup_NANOMIPS_JUMP_SLOT,
    fixup_NANOMIPS_IRELATIVE,
    fixup_NANOMIPS_PC25_S1,
    fixup_NANOMIPS_PC21_S1,
    fixup_NANOMIPS_PC14_S1,
    fixup_NANOMIPS_PC11_S1,
    fixup_NANOMIPS_PC10_S1,
    fixup_NANOMIPS_PC7_S1,
    fixup_NANOMIPS_PC4_S1,
    fixup_NANOMIPS_GPREL19_S2,
    fixup_NANOMIPS_GPREL18_S3,
    fixup_NANOMIPS_GPREL18,
    fixup_NANOMIPS_GPREL17_S1,
    fixup_NANOMIPS_GPREL16_S2,
    fixup_NANOMIPS_GPREL7_S2,
    fixup_NANOMIPS_GPREL_HI20,
    fixup_NANOMIPS_PCHI20,
    fixup_NANOMIPS_HI20,
    fixup_NANOMIPS_LO12,
    fixup_NANOMIPS_GPREL_I32,
    fixup_NANOMIPS_PC_I32,
    fixup_NANOMIPS_I32,
    fixup_NANOMIPS_GOT_DISP,
    fixup_NANOMIPS_GOTPC_I32,
    fixup_NANOMIPS_GOTPC_HI20,
    fixup_NANOMIPS_GOT_LO12,
    fixup_NANOMIPS_GOT_CALL,
    fixup_NANOMIPS_GOT_PAGE,
    fixup_NANOMIPS_GOT_OFST,
    fixup_NANOMIPS_LO4_S2,
    fixup_NANOMIPS_RESERVED1,
    fixup_NANOMIPS_GPREL_LO12,
    fixup_NANOMIPS_SCN_DISP,
    fixup_NANOMIPS_COPY,
    fixup_NANOMIPS_ALIGN,
    fixup_NANOMIPS_FILL,
    fixup_NANOMIPS_MAX,
    fixup_NANOMIPS_INSN32,
    fixup_NANOMIPS_FIXED,
    fixup_NANOMIPS_NORELAX,
    fixup_NANOMIPS_RELAX,
    fixup_NANOMIPS_SAVERESTORE,
    fixup_NANOMIPS_INSN16,
    fixup_NANOMIPS_JALR32,
    fixup_NANOMIPS_JALR16,
    fixup_NANOMIPS_JUMPTABLE_LOAD,
    fixup_NANOMIPS_FRAME_REG,
    fixup_NANOMIPS_NOTRAMP,
    // TLS relocations.
    fixup_NANOMIPS_TLS_DTPMOD,
    fixup_NANOMIPS_TLS_DTPREL,
    fixup_NANOMIPS_TLS_TPREL,
    fixup_NANOMIPS_TLS_GD,
    fixup_NANOMIPS_TLS_GD_I32,
    fixup_NANOMIPS_TLS_LD,
    fixup_NANOMIPS_TLS_LD_I32,
    fixup_NANOMIPS_TLS_DTPREL12,
    fixup_NANOMIPS_TLS_DTPREL16,
    fixup_NANOMIPS_TLS_DTPREL_I32,
    fixup_NANOMIPS_TLS_GOTTPREL,
    fixup_NANOMIPS_TLS_GOTTPREL_PC_I32,
    fixup_NANOMIPS_TLS_TPREL12,
    fixup_NANOMIPS_TLS_TPREL16,
    fixup_NANOMIPS_TLS_TPREL_I32,
    LastNanoMipsFixupKind = fixup_NANOMIPS_TLS_TPREL_I32,

    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };
} // namespace Mips
} // namespace llvm


#endif
