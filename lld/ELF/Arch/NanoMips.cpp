//===- NanoMips.cpp -------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "InputFiles.h"
#include "InputSection.h"
#include "NanoMipsTransformations.h"
#include "OutputSections.h"
#include "Symbols.h"
#include "SyntheticSections.h"
#include "Target.h"
#include "Thunks.h"
#include "lld/Common/ErrorHandler.h"
#include "lld/Common/Memory.h"
#include "llvm/Object/ELF.h"

using namespace llvm::object;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;
using namespace llvm;
using namespace llvm::support;

#define DEBUG_TYPE "lld-nanomips"

uint64_t elf::getNanoMipsPage(uint64_t expr) {
  return expr & ~static_cast<uint64_t>(0xFFF);
}

namespace {

// Helper functions

template <endianness E> void writeImmOf48bitIns(uint8_t *loc, uint64_t val) {
  // Different than the shuffle, the 48 bit instruction have
  // 32 bit imms at last two 16bit words in order so that
  // 15..0 goes first, and than the 31..16 portion, which
  // is opposite to big endian
  uint16_t *words = (uint16_t *)loc;
  if (E == support::big)
    std::swap(words[0], words[1]);

  write32(loc, val);

  if (E == support::big)
    std::swap(words[0], words[1]);
}

inline bool areNanoMips32BitFlagsSet(uint32_t eflags) {
  return ((eflags & EF_NANOMIPS_32BITMODE) != 0 ||
          ((eflags & EF_NANOMIPS_ARCH) == E_NANOMIPS_ARCH_32R6));
}

enum NanoMipsMach : uint32_t { NanoMipsIsa32R6 = 32, NanoMipsIsa64R6 = 64 };

// Check whether machine EXTENSION is an extension of machine BASE
inline bool doesNanoMipsMachExtend(uint32_t base, uint32_t extension) {
  // Copied logic from gold
  if (extension == base)
    return true;

  if ((base == NanoMipsMach::NanoMipsIsa32R6) &&
      doesNanoMipsMachExtend(NanoMipsMach::NanoMipsIsa64R6, extension))
    return true;

  return false;
}

// Return he MACH for a nanoMIPS e_flags value
uint32_t getNanoMipsMach(uint32_t eflags) {
  switch (eflags & EF_NANOMIPS_ARCH) {
  default:
  case E_NANOMIPS_ARCH_32R6:
    return NanoMipsMach::NanoMipsIsa32R6;
  case E_NANOMIPS_ARCH_64R6:
    return NanoMipsMach::NanoMipsIsa64R6;
  }
}

// used for: R_NANOMIPS_HI20, R_NANOMIPS_PC_HI20 and R_NANOMIPS_GPREL_HI20
template <endianness E> void writeValueHi20(uint8_t *loc, uint64_t val) {
  uint32_t instr = nanoMipsReadShuffle32<E>(loc);
  uint32_t data = (val & ~1) | ((val >> 31) & 1);
  data = (data & ~0xffc) | ((val >> 19) & 0xffc);
  uint32_t masked = (instr & ~0x1ffffd) | (data & 0x1ffffd);
  nanoMipsWriteShuffle32<E>(loc, masked);
}

// used for: R_NANOMIPS_PC4_S1 and R_NANOMIPS_GPREL7_S2
void writeValue16(uint8_t *loc, uint64_t val, uint8_t bitsSize, uint8_t shift) {
  uint32_t instr = read16(loc);
  uint32_t mask = (0xffff >> (16 - bitsSize)) << shift;
  uint32_t data = (instr & ~mask) | (val & mask);
  write16(loc, data);
}

// used for: R_NANOMIPS_PC10_S1 and R_NANOMIPS_PC7_S1
void writePcRel16(uint8_t *loc, uint64_t val, uint8_t bitsSize) {
  uint16_t instr = read16(loc);
  val = (val & ~1) | ((val >> bitsSize) & 1);
  uint16_t mask = (0xffff >> (16 - bitsSize));
  uint16_t data = (instr & ~mask) | (val & mask);
  write16(loc, data);
}

// used for: R_NANOMIPS_PC25_S1, R_NANOMIPS_PC21_S1, R_NANOMIPS_PC14_S1 and
// R_NANOMIPS_PC11_S1
template <endianness E>
void writePcRel32(uint8_t *loc, uint64_t val, uint8_t bitsSize) {
  uint32_t instr = nanoMipsReadShuffle32<E>(loc);
  val = (val & ~1) | ((val >> bitsSize) & 1);
  uint32_t mask = (0xffffffff >> (32 - bitsSize));
  uint32_t data = (instr & ~mask) | (val & mask);
  nanoMipsWriteShuffle32<E>(loc, data);
}

// used for: R_NANOMIPS_LO12, R_NANOMIPS_GPREL19_S2, R_NANOMIPS_GPREL18,
// R_NANOMIPS_GPREL17_S1 and R_NANOMIPS_GPREL_LO12
template <endianness E>
void writeValue32Shifted(uint8_t *loc, uint64_t val, uint8_t bitsSize,
                         uint8_t shift) {
  uint32_t instr = nanoMipsReadShuffle32<E>(loc);
  uint32_t mask = (0xffffffff >> (32 - bitsSize)) << shift;
  uint32_t data = (instr & ~mask) | (val & mask);
  nanoMipsWriteShuffle32<E>(loc, data);
}
// used for checking values of: pcrel relocations and gprel ones also
// R_NANOMIPS_LO12
void checkVal(uint8_t *loc, int64_t v, int n, const Relocation &rel,
              const Symbol *sym, uint32_t shift, bool signedVal) {

  if ((v & ((1 << shift) - 1)) != 0) {
    error(getErrorLocation(loc) + "\tvalue: \t" + llvm::utohexstr(v) +
          "\tlast N bits have to be zero in all S{N} relocations");
  }

  if (sym && sym->isUndefWeak())
    return;
  if (signedVal)
    checkInt(loc, v, n, rel);
  else
    checkUInt(loc, v, n, rel);
}

bool isPlaceholderReloc(const RelType relType) {
  switch (relType) {
  case R_NANOMIPS_NONE:
  case R_NANOMIPS_ALIGN:
  case R_NANOMIPS_FILL:
  case R_NANOMIPS_MAX:
  case R_NANOMIPS_INSN32:
  case R_NANOMIPS_FIXED:
  case R_NANOMIPS_NORELAX:
  case R_NANOMIPS_RELAX:
  case R_NANOMIPS_SAVERESTORE:
  case R_NANOMIPS_INSN16:
  case R_NANOMIPS_JALR32:
  case R_NANOMIPS_JALR16:
  case R_NANOMIPS_JUMPTABLE_LOAD:
    return true;
  default:
    return false;
  }
}

// TODO: Support for other endianess, and bit size, now it is only Little Endian
// 32 bit
template <class ELFT> class NanoMips final : public TargetInfo {
public:
  NanoMips();

  RelExpr getRelExpr(RelType type, const Symbol &s,
                     const uint8_t *loc) const override;

  void relocate(uint8_t *loc, const Relocation &rel,
                uint64_t val) const override;

  bool relaxOnce(int pass) const override {
    return this->transformController.relaxOnce(pass);
  }
  void relocateAlloc(InputSectionBase &sec, uint8_t *buf) const override;

  uint32_t calcEFlags() const override;

private:
  NanoMipsTransformController<ELFT> transformController;
};
} // namespace

template <class ELFT> NanoMips<ELFT>::NanoMips() {

  assert(config->nanoMipsExpandReg >= 0 && config->nanoMipsExpandReg < 32 &&
         "nanoMIPS regs range from 0 to 32");
  //  TODO: When needed initialize symbolicRel, iRelativeRel, relativeRel, etc.
  copyRel = R_NANOMIPS_COPY;
  defaultMaxPageSize = 65536;
  this->transformController.initState();
}

template <class ELFT>
RelExpr NanoMips<ELFT>::getRelExpr(RelType type, const Symbol &s,
                                   const uint8_t *loc) const {
  // TODO: Other relocations like R_NANOMIPS_GOT*, R_NANOMIPS_TLS*
  // and other remaining
  switch (type) {
  case R_NANOMIPS_PC25_S1:
  case R_NANOMIPS_PC21_S1:
  case R_NANOMIPS_PC14_S1:
  case R_NANOMIPS_PC11_S1:
  case R_NANOMIPS_PC10_S1:
  case R_NANOMIPS_PC7_S1:
  case R_NANOMIPS_PC4_S1:
  case R_NANOMIPS_PC_I32:
  case R_NANOMIPS_PC32:
    return R_PC;
  case R_NANOMIPS_NEG:
    return R_NANOMIPS_NEG_COMPOSITE;
  case R_NANOMIPS_ASHIFTR_1:
    return R_NANOMIPS_ASHIFTR;
  case R_NANOMIPS_32:
  case R_NANOMIPS_UNSIGNED_8:
  case R_NANOMIPS_SIGNED_8:
  case R_NANOMIPS_UNSIGNED_16:
  case R_NANOMIPS_SIGNED_16:
  case R_NANOMIPS_LO12:
  case R_NANOMIPS_I32:
  case R_NANOMIPS_HI20:
  case R_NANOMIPS_LO4_S2:
    return R_ABS;
  case R_NANOMIPS_PC_HI20:
    return R_NANOMIPS_PAGE_PC;
  case R_NANOMIPS_GPREL19_S2:
  case R_NANOMIPS_GPREL18:
  case R_NANOMIPS_GPREL17_S1:
  case R_NANOMIPS_GPREL_I32:
  case R_NANOMIPS_GPREL_HI20:
  case R_NANOMIPS_GPREL_LO12:
  case R_NANOMIPS_GPREL7_S2:
    return R_NANOMIPS_GPREL;
  case R_NANOMIPS_NOTRAMP:
  case R_NANOMIPS_NONE:
    return R_NONE;

  case R_NANOMIPS_ALIGN:
  case R_NANOMIPS_MAX:
  case R_NANOMIPS_FILL:
  case R_NANOMIPS_SAVERESTORE:
  case R_NANOMIPS_NORELAX:
  case R_NANOMIPS_RELAX:
  case R_NANOMIPS_FIXED:
  case R_NANOMIPS_INSN32:
  case R_NANOMIPS_INSN16:
    // Used to save these relocations in relocation vector as
    // R_NONE relocs are discareded from this vector, and
    // these relocs are needed for relaxations/transformations
    return R_RELAX_HINT;
  default:
    error(getErrorLocation(loc) + "unknown relocation (" + Twine(type) +
          ") against symbol " + toString(s) +
          " loc: " + llvm::utohexstr(uint64_t(loc)) +
          " file name: " + toString(s.file->getName()));
    return R_NONE;
  }
}

template <class ELFT>
void NanoMips<ELFT>::relocate(uint8_t *loc, const Relocation &rel,
                              uint64_t val) const {
  switch (rel.type) {
  case R_NANOMIPS_PC_I32:
    checkInt(loc, val - 4, 32, rel);
    writeImmOf48bitIns<ELFT::TargetEndianness>(loc, val - 4);
    break;
  case R_NANOMIPS_PC32:
  case R_NANOMIPS_32:
  case R_NANOMIPS_I32:
  case R_NANOMIPS_GPREL_I32:
    checkInt(loc, val, 32, rel);
    writeImmOf48bitIns<ELFT::TargetEndianness>(loc, val);
    break;
  case R_NANOMIPS_NEG:
  case R_NANOMIPS_ASHIFTR_1:
    if (ELFT::Is64Bits)
      write64(loc, val);
    else
      write32(loc, val);
    break;
  case R_NANOMIPS_NONE:
  case R_NANOMIPS_FIXED:
  case R_NANOMIPS_ALIGN:
  case R_NANOMIPS_INSN16:
  case R_NANOMIPS_INSN32:
  case R_NANOMIPS_FILL:
  case R_NANOMIPS_MAX:
  case R_NANOMIPS_SAVERESTORE:
  case R_NANOMIPS_RELAX:
  case R_NANOMIPS_NORELAX:
  case R_NANOMIPS_NOTRAMP:
    break;
  case R_NANOMIPS_UNSIGNED_16:
    checkUInt(loc, val, 16, rel);
    write16(loc, val);
    break;
  case R_NANOMIPS_SIGNED_16:
    checkInt(loc, val, 16, rel);
    write16(loc, val);
    break;
  case R_NANOMIPS_HI20:
  case R_NANOMIPS_PC_HI20:
  case R_NANOMIPS_GPREL_HI20:
    checkInt(loc, val, 32, rel);
    writeValueHi20<ELFT::TargetEndianness>(loc, val);
    break;
  case R_NANOMIPS_PC4_S1: {
    checkVal(loc, val - 2, 5, rel, rel.sym, 1, false);
    val = (val - 2) >> 1;
    writeValue16(loc, val, 4, 0);
    break;
  }
  case R_NANOMIPS_SIGNED_8: {
    checkInt(loc, val, 8, rel);
    write8(loc, val);
    break;
  }
  case R_NANOMIPS_UNSIGNED_8:
    checkUInt(loc, val, 8, rel);
    write8(loc, val);
    break;
  case R_NANOMIPS_GPREL7_S2:
    checkVal(loc, val, 9, rel, rel.sym, 2, false);
    writeValue16(loc, val >> 2, 7, 0);
    break;
  case R_NANOMIPS_PC10_S1:
    checkVal(loc, val - 2, 11, rel, rel.sym, 1, true);
    writePcRel16(loc, val - 2, 10);
    break;
  case R_NANOMIPS_PC7_S1:
    checkVal(loc, val - 2, 8, rel, rel.sym, 1, true);
    writePcRel16(loc, val - 2, 7);
    break;
  case R_NANOMIPS_PC25_S1: {
    checkVal(loc, val - 4, 26, rel, rel.sym, 1, true);
    writePcRel32<ELFT::TargetEndianness>(loc, val - 4, 25);
    break;
  }
  case R_NANOMIPS_PC21_S1:
    checkVal(loc, val - 4, 22, rel, rel.sym, 1, true);
    writePcRel32<ELFT::TargetEndianness>(loc, val - 4, 21);
    break;
  case R_NANOMIPS_PC14_S1:
    checkVal(loc, val - 4, 15, rel, rel.sym, 1, true);
    writePcRel32<ELFT::TargetEndianness>(loc, val - 4, 14);
    break;
  case R_NANOMIPS_PC11_S1:
    checkVal(loc, val - 4, 12, rel, rel.sym, 1, true);
    writePcRel32<ELFT::TargetEndianness>(loc, val - 4, 11);
    break;
  case R_NANOMIPS_LO12:
    writeValue32Shifted<ELFT::TargetEndianness>(loc, val, 12, 0);
    break;
  case R_NANOMIPS_LO4_S2:
    checkVal(loc, val & 0xfff, 6, rel, rel.sym, 2, false);
    writeValue16(loc, val >> 2, 4, 0);
    break;
  case R_NANOMIPS_GPREL19_S2:
    checkVal(loc, val, 21, rel, rel.sym, 2, false);
    writeValue32Shifted<ELFT::TargetEndianness>(loc, val, 19, 2);
    break;
  case R_NANOMIPS_GPREL18:
    checkUInt(loc, val, 18, rel);
    writeValue32Shifted<ELFT::TargetEndianness>(loc, val, 18, 0);
    break;
  case R_NANOMIPS_GPREL17_S1:
    checkVal(loc, val, 18, rel, rel.sym, 1, false);
    writeValue32Shifted<ELFT::TargetEndianness>(loc, val, 17, 1);
    break;
  case R_NANOMIPS_GPREL_LO12:
    writeValue32Shifted<ELFT::TargetEndianness>(loc, val, 12, 0);
    break;
  default:
    llvm_unreachable("unknown relocation");
    break;
  }
}

template <class ELFT> TargetInfo *elf::getNanoMipsTargetInfo() {
  static NanoMips<ELFT> t;
  return &t;
}

template <class ELFT>
void NanoMips<ELFT>::relocateAlloc(InputSectionBase &sec, uint8_t *buf) const {
  const unsigned bits = config->is64 ? 64 : 32;
  uint64_t secAddr = sec.getOutputSection()->addr;
  if (auto *s = dyn_cast<InputSection>(&sec))
    secAddr += s->outSecOff;
  uint64_t valFromBefore = 0;
  bool prevRelocOnlyCalculating = false;
  for (auto [i, rel] : llvm::enumerate(sec.relocs())) {

    if (rel.expr == R_RELAX_HINT)
      continue;

    uint8_t *loc = buf + rel.offset;
    uint64_t val = 0;
    if (!prevRelocOnlyCalculating) {
      val = SignExtend64(sec.getRelocTargetVA(sec.file, rel.type, rel.addend,
                                              secAddr + rel.offset, *rel.sym,
                                              rel.expr),
                         bits);
    } else {
      // Previous relocation was part of a composite relocation
      // (see comment below), use its calculated value as this
      // relocation's addend
      // TODO: See if maybe addend of the current relocation
      // should be added to the previous reloc's value instead of
      // just ignoring it
      val = SignExtend64(sec.getRelocTargetVA(sec.file, rel.type, valFromBefore,
                                              secAddr + rel.offset, *rel.sym,
                                              rel.expr),
                         bits);
    }

    // nanoMIPS has composite relocations, several relocations
    // in a row referring to the same location. Then the value is
    // calculated by combining (adding) the results of all the
    // relocations
    if (i + 1 != sec.relocs().size() &&
        sec.relocs()[i + 1].offset == rel.offset &&
        !isPlaceholderReloc(sec.relocs()[i + 1].type)) {
      prevRelocOnlyCalculating = true;
      valFromBefore = val;
      continue;
    }

    prevRelocOnlyCalculating = false;
    relocate(loc, rel, val);
  }
}

template <class ELFT> uint32_t NanoMips<ELFT>::calcEFlags() const {
  // TODO: This function will need testing once dynamic linking is started
  uint32_t retFlags = 0;
  if (ctx.objectFiles.size() == 0)
    return retFlags;

  // Set flags from first obj file
  retFlags =
      cast<ObjFile<ELFT>>(ctx.objectFiles[0])->getObj().getHeader().e_flags;

  if ((retFlags & EF_NANOMIPS_PIC) == 0 &&
      (config->pie || config->shared || ctx.sharedFiles.size() != 0) &&
      !config->relocatable)
    error(ctx.objectFiles[0]->getName() +
          ": non-PIC object found in dynamic link, recompile with -fpic");

  // Iterate through others and merge
  for (size_t i = 1; i < ctx.objectFiles.size(); i++) {
    InputFile *f = ctx.objectFiles[i];
    uint32_t newFlags = cast<ObjFile<ELFT>>(f)->getObj().getHeader().e_flags;
    uint32_t oldFlags = retFlags;

    if ((newFlags & EF_NANOMIPS_PIC) == 0 &&
        (config->pie || config->shared || ctx.sharedFiles.size() != 0) &&
        !config->relocatable)
      error(f->getName() +
            ": non-PIC object found in dynamic link, recompile with -fpic");

    if (newFlags == oldFlags)
      continue;

    // If new flags do not contain pid, pcrel, linkrelax or pic, the
    // missing should be removed from the final flags
    retFlags &= newFlags | ~(EF_NANOMIPS_PID | EF_NANOMIPS_PCREL |
                             EF_NANOMIPS_LINKRELAX | EF_NANOMIPS_PIC);

    // Exclude the previous flags from old and new
    oldFlags &= ~(EF_NANOMIPS_PID | EF_NANOMIPS_PCREL | EF_NANOMIPS_LINKRELAX |
                  EF_NANOMIPS_PIC);
    newFlags &= ~(EF_NANOMIPS_PID | EF_NANOMIPS_PCREL | EF_NANOMIPS_LINKRELAX |
                  EF_NANOMIPS_PIC);

    // Compare the ISAs
    if (areNanoMips32BitFlagsSet(oldFlags) !=
        areNanoMips32BitFlagsSet(newFlags))
      error(f->getName() + ": Linking 32-bit code with 64-bit code");

    else if (!doesNanoMipsMachExtend(getNanoMipsMach(newFlags),
                                     getNanoMipsMach(oldFlags))) {

      if (doesNanoMipsMachExtend(getNanoMipsMach(oldFlags),
                                 getNanoMipsMach(newFlags))) {
        // Copy the architecture info from new to final flags. Also copy
        // the 32-bit flag (if set) so that we continue to recognise
        // output as a 32-bit binary.
        retFlags &= ~(EF_NANOMIPS_ARCH | EF_NANOMIPS_MACH);

        retFlags |= (newFlags & (EF_NANOMIPS_ARCH | EF_NANOMIPS_MACH |
                                 EF_NANOMIPS_32BITMODE));

      } else {
        // ISA's are incompatible
        error(f->getName() +
              ": Linking incompatible machine modules than the previous ones!");
      }
    }

    // Exlude the previous flags from old and new
    newFlags &= ~(EF_NANOMIPS_ARCH | EF_NANOMIPS_MACH | EF_NANOMIPS_32BITMODE);

    oldFlags &= ~(EF_NANOMIPS_ARCH | EF_NANOMIPS_MACH | EF_NANOMIPS_32BITMODE);

    // Compare ABIs
    if ((newFlags & EF_NANOMIPS_ABI) != (oldFlags & EF_NANOMIPS_ABI)) {
      // Error if both are set differently
      if ((newFlags & EF_NANOMIPS_ABI) != 0 &&
          (oldFlags & EF_NANOMIPS_ABI) != 0)
        error(f->getName() +
              ": ABI mismatch, different ABI than the previous ones!");

      newFlags &= ~EF_NANOMIPS_ABI;
      oldFlags &= ~EF_NANOMIPS_ABI;
    }

    // Other mismatches
    if (newFlags != oldFlags)
      error(f->getName() + ": uses different e_flags (0x" +
            utohexstr(newFlags, false, 8) + ") than currently calculated (0x" +
            utohexstr(oldFlags, false, 8) + ")");
  }

  // Clear some flags, depending on the output
  // TODO: relocatable + finalize relocs (not yet implemented from gold)

  if (!config->relocatable) {
    retFlags &= ~(EF_NANOMIPS_PID | EF_NANOMIPS_PCREL | EF_NANOMIPS_LINKRELAX);

    // Keep PIC bit only for position independent output
    if (!config->pie && !config->shared)
      retFlags &= ~EF_NANOMIPS_PIC;
  }

  return retFlags;
}

template TargetInfo *elf::getNanoMipsTargetInfo<ELF32LE>();
template TargetInfo *elf::getNanoMipsTargetInfo<ELF32BE>();
template TargetInfo *elf::getNanoMipsTargetInfo<ELF64LE>();
template TargetInfo *elf::getNanoMipsTargetInfo<ELF64BE>();
