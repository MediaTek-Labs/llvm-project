//===--- NanoMipsABIFlags.h - MIPS ABI flags ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the constants for the ABI flags structure contained
// in the .nanoMIPS.abiflags section.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SUPPORT_NANOMIPSABIFLAGS_H
#define LLVM_SUPPORT_NANOMIPSABIFLAGS_H

namespace llvm {

namespace NanoMips {

// Values for the xxx_size bytes of an ABI flags structure.        
enum AFL_REG {
    AFL_REG_NONE = 0x00,
    AFL_REG_32 = 0x01,
    AFL_REG_64 = 0x02,
    AFL_REG_128 = 0x03
};

// Values for the ASEs field of an ABI flags structure.
enum AFL_ASE {
    AFL_ASE_TLB = 0x00000001, // TLB control
    AFL_ASE_EVA = 0x00000004, // Enhanced VA Scheme
    AFL_ASE_MCU = 0x00000008, // MCU (MicroController) extenstion
    AFL_ASE_MT = 0x00000040, // Multi-threading extension
    AFL_ASE_VIRT = 0x00000100, // Virtualization extension
    AFL_ASE_MSA = 0x00000200, // MSA extension
    AFL_ASE_RESERVED1 = 0x00000400, // was MIPS16 ASE
    AFL_ASE_RESERVED2 = 0x00000800, // was MICROMIPS ASE
    AFL_ASE_DSPR3 = 0x00002000, // DSP R3 extension
    AFL_ASE_CRC = 0x00008000, // Cyclic Redundancy Check extenstion
    AFL_ASE_GINV = 0x00020000, // Gloval INValidate extension
    AFL_ASE_xNMS = 0x00040000, // Full base ISA, not the nanoMIPS subset 

};

enum VAL_GNU_NANOMIPS_ABI_FP {
    VAL_GNU_NANOMIPS_ABI_FP_ANY = 0, // Not specified
    VAL_GNU_NANOMIPS_ABI_FP_DOUBLE = 1, // Double-precision hard float
    VAL_GNU_NANOMIPS_ABI_FP_SINGLE = 2, // Single-precision hard float
    VAL_GNU_NANOMIPS_ABI_FP_SOFT = 3, // Soft float
};

enum AFL_EXT {    
    AFL_EXT_NONE = 0 // No processor specific extension
};

}

}

#endif