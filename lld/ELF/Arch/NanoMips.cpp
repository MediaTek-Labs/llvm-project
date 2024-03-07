//===- NanoMips.cpp -------------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "InputFiles.h"
#include "OutputSections.h"
#include "Symbols.h"
#include "SyntheticSections.h"
#include "Target.h"
#include "Thunks.h"
#include "lld/Common/ErrorHandler.h"
#include "llvm/Object/ELF.h"
#include "InputSection.h"
#include "llvm/Support/Endian.h"
#include "Arch/NanoMipsProperties.h"
#include "SyntheticSections.h"
#include "lld/Common/Memory.h"

using namespace llvm::object;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;
using namespace llvm;
using namespace llvm::support;

// use --mllvm with --debug or --debug-only=<name>
#define DEBUG_TYPE "lld-nanomips"

template <endianness E>
static uint32_t readShuffle32(const uint8_t *loc)
{
  // Similar to microMIPS, little endian instructions are encoded as
  // big endian so that the opcode comes first and that the hardware could
  // know sooner if it is a 16bit, 32bit or 48bit instruction
  uint32_t v = read32(loc);
  if(E == support::little)
    return (v << 16) | (v >> 16);
  return v;
}


template <endianness E>
static void writeShuffle32(uint8_t *loc, uint64_t val)
{
  uint16_t *words = (uint16_t *)loc;
  if(E == support::little)
    std::swap(words[0], words[1]);
  
  write32(loc, val);

  if(E == support::little)
    std::swap(words[0], words[1]);
}

template <endianness E>
static void writeImm48bitIns(uint8_t *loc, uint64_t val)
{
  // Different than the shuffle, the 48 bit instruction have
  // 32 bit imms at last two 16bit words in order so that
  // 15..0 goes first, and than the 31..16 portion, which
  // is opposite to big endian
    uint16_t *words = (uint16_t *)loc;
  if(E == support::big)
    std::swap(words[0], words[1]);
  
  write32(loc, val);

  if(E == support::big)
    std::swap(words[0], words[1]);
}

template <endianness E>
static uint64_t readInsn(ArrayRef<uint8_t> data, uint64_t off, uint32_t insnSize)
{
  assert(off + insnSize <= data.size() && "Overflow on buffer in readInsn");
  if(insnSize == 6) return read16(&data[off]);
  else if(insnSize == 4) return readShuffle32<E>(&data[off]);
  else if(insnSize == 2) return read16(&data[off]);
  else llvm_unreachable("Unknown byte size of nanoMIPS instruction (only 2, 4 and 6 known)");
}

template <endianness E>
static void writeInsn(uint64_t insn, ArrayRef<uint8_t>data, uint64_t off, uint32_t insnSize)
{
    assert(off + insnSize <= data.size() && "Overflow on buffer in writeInsn");
    uint8_t *dataPtr = const_cast<uint8_t *>(data.begin());
    if (insnSize == 6) write16(dataPtr + off, (uint16_t)insn);
    else if(insnSize == 4) writeShuffle32<E>(dataPtr + off,((uint32_t)insn));
    else if(insnSize == 2) write16(dataPtr + off, (uint16_t)insn);
    else llvm_unreachable("Unknown byte size of nanoMIPS instruction (only 2, 4, and 6 known)");
}

uint64_t elf::getNanoMipsPage(uint64_t expr) {
  return expr & ~static_cast<uint64_t>(0xFFF);
}

template<class ELFT>
static bool isNanoMipsPcRel(const ObjFile<ELFT> *obj)
{
  return (obj->getObj().getHeader().e_flags & llvm::ELF::EF_NANOMIPS_PCREL) != 0;
}

namespace {

  
// TODO: Support for other endianess, and bit size, now it is Little Endian 32 bit
template <class ELFT>
class NanoMips final : public TargetInfo {
public:
    NanoMips();
    
    RelExpr getRelExpr(RelType type, const Symbol &s,
                     const uint8_t *loc) const override;

    void relocate(uint8_t *loc, const Relocation &rel,
                uint64_t val) const override;

    bool mayRelax() const override;
    bool relaxOnce(int pass) const override;

    // TODO:
    // uint32_t calcEFlags() const override;
private:
    NanoMipsRelocPropertyTable relocPropertyTable;
    NanoMipsInsPropertyTable insPropertyTable;
    // Needs to be declared after insPropertyTable
    NanoMipsTransformController currentTransformation;

    bool safeToModify(InputSection *sec) const;

    // relax + expand
    void transform(InputSection *sec) const;
    void initTransformAuxInfo() const;
    void align(InputSection *sec, Relocation &reloc, uint32_t relNum) const;
    void finalizeRelaxations() const override;
    // bool relax(InputSection *sec) const;
    // bool expand(InputSection *sec) const;
};
} // namespace

template <class ELFT>
NanoMips<ELFT>::NanoMips(): currentTransformation(&insPropertyTable) {
  // assert(!ELFT::Is64Bits() && ELF32LE::TargetEndianness() == llvm::support::endianness::little
  //         && "32 little endian is the only supported target for nanoMIPS for now");
  assert(config->nanoMipsExpandReg >= 0 && config->nanoMipsExpandReg < 32 && "nanoMIPS regs range from 0 to 32");
  copyRel = R_NANOMIPS_COPY;
  // noneRel Already zero, and is now static constexpr
  // noneRel = R_NANOMIPS_NONE;
  defaultMaxPageSize = 65536;
  pltEntrySize = 0;

  LLVM_DEBUG(
  llvm::dbgs() << "Current reloc properties:\n" << relocPropertyTable.toString() << "\n\n\n";
  llvm::dbgs() << "Current instruction properties:\n" << insPropertyTable.toString() << "\n\n\n";
  llvm::dbgs() << "relax_lo12: " << config->nanoMipsRelaxLo12 << "\n";
  llvm::dbgs() << "insn32: " << config->nanoMipsInsn32 << "\n";
  llvm::dbgs() << "fix_nmips_hw110880: " << config->nanoMipsFixHw110880 << "\n"; 
  llvm::dbgs() << "expand_reg: " << config->nanoMipsExpandReg << "\n";
  llvm::dbgs() << "strict_address_modes: " << config->nanoMipsStrictAddressModes << "\n";
  );
  this->currentTransformation.initState();
}

//used for: R_NANOMIPS_HI20, R_NANOMIPS_PC_HI20 and R_NANOMIPS_GPREL_HI20
template <endianness E>
static void writeValueHi20(uint8_t *loc, uint64_t val) {
  uint32_t instr = readShuffle32<E>(loc);
  // instr = bswap(instr);
  uint32_t data = (val & ~1) | ((val >> 31) & 1);
  data = (data & ~0xffc) | ((val >> 19) & 0xffc); 
  uint32_t masked = (instr & ~0x1ffffd) | (data & 0x1ffffd);
  // masked = bswap(masked);
  writeShuffle32<E>(loc, masked);
}

//used for: R_NANOMIPS_PC4_S1 and R_NANOMIPS_GPREL_S2
static void writeValue16(uint8_t *loc, uint64_t val, uint8_t bitsSize,
                       uint8_t shift) {
  uint32_t instr = read16(loc);
  uint32_t mask = (0xffff >> (16 - bitsSize)) << shift;
  uint32_t data = (instr & ~mask) | (val & mask);
  write16(loc, data);
}

//used for: R_NANOMIPS_PC10_S1 and R_NANOMIPS_PC7_S1
static void writePcRel16(uint8_t *loc, uint64_t val, uint8_t bitsSize) {
  uint16_t instr = read16(loc);
  val = (val & ~1) | ((val >> bitsSize) & 1);
  uint16_t mask = (0xffff >> (16 - bitsSize));
  uint16_t data = (instr & ~mask) | (val & mask);
  write16(loc, data);
}

//used for: R_NANOMIPS_PC25_S1, R_NANOMIPS_PC21_S1, R_NANOMIPS_PC14_S1 and R_NANOMIPS_PC11_S1
template <endianness E>
static void writePcRel32(uint8_t *loc, uint64_t val, uint8_t bitsSize) {
    uint32_t instr = readShuffle32<E>(loc); 
    // instr = bswap(instr);
    val = (val & ~1) | ((val >> bitsSize) & 1);
    uint32_t mask = (0xffffffff >> (32 - bitsSize));
    uint32_t data = (instr & ~mask) | (val & mask);
    // data = bswap(data);
    writeShuffle32<E>(loc, data);
}

//used for: R_NANOMIPS_LO12, R_NANOMIPS_GPREL19_S2, R_NANOMIPS_GPREL18, R_NANOMIPS_GPREL17_S1 and R_NANOMIPS_GPREL_LO12
template <endianness E>
static void writeValue32be(uint8_t *loc, uint64_t val, uint8_t bitsSize,
                       uint8_t shift) {
    uint32_t instr = readShuffle32<E>(loc); 
    // instr = bswap(instr);
    uint32_t mask = (0xffffffff >> (32 - bitsSize)) << shift;
    uint32_t data = (instr & ~mask) | (val & mask);
    // data = bswap(data);
    writeShuffle32<E>(loc, data);
}

void checkIntPcRel(uint8_t *loc, int64_t v, int n, const Relocation &rel, const Symbol &sym) {
  if ((v & 1) != 0)
    error(getErrorLocation(loc) + "\tvalue: \t" + llvm::utohexstr(v) + "\tlast bit has to be zero in all PC_REL \n");
  if(sym.isUndefWeak()) //if symbol is weak, then we don't have to check it's range
    return;
  else if (rel.type == R_NANOMIPS_PC4_S1)
    checkUInt(loc, v, n, rel); // R_NANOMIPS_PC4_S1 is unsigned 5-bit integer
  else
    checkInt(loc, v, n, rel); //we have to check if value v fits in signed n-bit integer
}

template<class ELFT>
RelExpr NanoMips<ELFT>::getRelExpr(RelType type, const Symbol &s,
                            const uint8_t *loc) const {
  switch (type){
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
  case R_NANOMIPS_32:
  case R_NANOMIPS_UNSIGNED_8:
  case R_NANOMIPS_SIGNED_8:
  case R_NANOMIPS_UNSIGNED_16:
  case R_NANOMIPS_SIGNED_16:
  case R_NANOMIPS_LO12: 
  case R_NANOMIPS_ASHIFTR_1:
  case R_NANOMIPS_I32:
  case R_NANOMIPS_HI20:
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
  case R_NANOMIPS_NONE:
  case R_NANOMIPS_FIXED:
  case R_NANOMIPS_INSN32:
  case R_NANOMIPS_INSN16:
    return R_NONE;
  
  case R_NANOMIPS_ALIGN:
  case R_NANOMIPS_MAX:
  case R_NANOMIPS_FILL:
    // Used to save R_NANOMIPS_ALIGN in relocation vector
    // TODO: See if this is only needed for relaxations and expansions
    // so maybe it could be relaxed to R_NONE in that case
    return R_RELAX_HINT;
  default:
    error(getErrorLocation(loc) + "unknown relocation (" + Twine(type) +
          ") against symbol " + toString(s) + " loc: " + llvm::utohexstr(uint64_t(loc)) + " file name: " + toString(s.file->getName()) );
    return R_NONE;
  }                   
}

template <class ELFT>
void NanoMips<ELFT>::relocate(uint8_t *loc, const Relocation &rel, uint64_t val) const {
  switch (rel.type) {
  case R_NANOMIPS_PC_I32:
    checkInt(loc, val - 4, 32, rel);
    writeImm48bitIns<ELFT::TargetEndianness>(loc, val - 4);
    break;
  case R_NANOMIPS_PC32:
  case R_NANOMIPS_32:
  case R_NANOMIPS_I32:
  case R_NANOMIPS_GPREL_I32:
    checkInt(loc, val, 32, rel);
    writeImm48bitIns<ELFT::TargetEndianness>(loc, val);
    break;
  case R_NANOMIPS_NEG:
  case R_NANOMIPS_ASHIFTR_1: 
    assert(0 && "Incorrect logic for R_NANOMIPS_NEG and R_NANOMIPS_ASHIFTR_1");
    break;
  case R_NANOMIPS_NONE:
  case R_NANOMIPS_FIXED:
  case R_NANOMIPS_ALIGN:
  case R_NANOMIPS_INSN16:
  case R_NANOMIPS_INSN32:
  case R_NANOMIPS_FILL:
  case R_NANOMIPS_MAX:
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
  case R_NANOMIPS_PC4_S1:{
    checkIntPcRel(loc, val - 2, 5, rel, *rel.sym);
    val = (val - 2) >> 1;
    writeValue16(loc, val, 4, 0);
    break;
  }
  case R_NANOMIPS_SIGNED_8:{
    checkInt(loc, val, 8, rel);
    write8(loc, val);
    break;
  }
  case R_NANOMIPS_UNSIGNED_8:
    checkUInt(loc, val, 8, rel);
    write8(loc, val);
    break;
  case R_NANOMIPS_GPREL7_S2:
    checkUInt(loc, val, 9, rel);
    writeValue16(loc, val, 7, 2);
    break;
  case R_NANOMIPS_PC10_S1:
    checkIntPcRel(loc, val - 2, 11, rel, *rel.sym);
    writePcRel16(loc, val - 2, 10);
    break;
  case R_NANOMIPS_PC7_S1:
    checkIntPcRel(loc, val - 2, 8, rel, *rel.sym);
    writePcRel16(loc, val - 2, 7);
    break;
  case R_NANOMIPS_PC25_S1:
    {
    checkIntPcRel(loc, val - 4, 26, rel, *rel.sym);
    writePcRel32<ELFT::TargetEndianness>(loc, val - 4, 25);
    break;
    }
  case R_NANOMIPS_PC21_S1:
    checkIntPcRel(loc, val - 4, 22, rel, *rel.sym);
    writePcRel32<ELFT::TargetEndianness>(loc, val - 4, 21);
    break;
  case R_NANOMIPS_PC14_S1:
    checkIntPcRel(loc, val - 4, 15, rel, *rel.sym);
    writePcRel32<ELFT::TargetEndianness>(loc, val - 4, 14);
    break;
  case R_NANOMIPS_PC11_S1:
    checkIntPcRel(loc, val - 4, 12, rel, *rel.sym);
    writePcRel32<ELFT::TargetEndianness>(loc, val - 4, 11);
    break;
  case R_NANOMIPS_LO12:
    writeValue32be<ELFT::TargetEndianness>(loc, val, 12, 0);
    break;
  case R_NANOMIPS_GPREL19_S2:
    checkUInt(loc, val, 21, rel);
    writeValue32be<ELFT::TargetEndianness>(loc, val, 19, 2);
    break;
  case R_NANOMIPS_GPREL18:
    checkUInt(loc, val, 18, rel);
    writeValue32be<ELFT::TargetEndianness>(loc, val, 18, 0);
    break;
  case R_NANOMIPS_GPREL17_S1:
    checkUInt(loc, val, 18, rel);
    writeValue32be<ELFT::TargetEndianness>(loc, val, 17, 1);
    break;
  case R_NANOMIPS_GPREL_LO12:
    writeValue32be<ELFT::TargetEndianness>(loc, val, 12, 0);
    break;
  default:
    llvm_unreachable("unknown relocation");
    break;
  }
}

template <class ELFT>
TargetInfo *elf::getNanoMipsTargetInfo() {
    static NanoMips<ELFT> t;
    return &t;
}

template <class ELFT>
bool NanoMips<ELFT>::mayRelax() const
{
  // TODO: When the finalize-relocs option is added, change this expression
  // also goes for sort-by-reference option
  return (!config->relocatable && (config->relax || config->expand));
}

template <class ELFT>
bool NanoMips<ELFT>::safeToModify(InputSection *sec) const
{
  bool modifiable = false;
  if(auto *obj = sec->getFile<ELF32LE>())
  {
    modifiable = (obj->getObj().getHeader().e_flags & EF_NANOMIPS_LINKRELAX) != 0;
  }
  return modifiable;
}

// TODO: Emit reloc option, somewhat different transformations
template <class ELFT>
bool NanoMips<ELFT>::relaxOnce(int pass) const
{
  if(pass == 0)
  {
    initTransformAuxInfo();
  }
  if(this->currentTransformation.isNone()) return false;
  LLVM_DEBUG(llvm::dbgs() << "Transformation Pass num: " << pass << "\n";);
  // TODO: Should full nanoMips ISA be checked as full or per obj, as it is checked
  bool changed = false;
  if(this->mayRelax())
  {
    for(OutputSection *osec : outputSections)
    {
      if((osec->flags & (SHF_EXECINSTR | SHF_ALLOC)) != (SHF_EXECINSTR | SHF_ALLOC) ||
          !(osec->type & SHT_PROGBITS))
          continue;
      // TODO: There are null input sections, sections in gold, see what's up with that later
      for(InputSection *sec : getInputSections(osec))
      {
        if(!this->safeToModify(sec)) continue;

        if(sec->numRelocations) this->transform(sec);

      }
    }

    changed = this->currentTransformation.shouldRunAgain();
    const_cast<NanoMipsTransformController &>(this->currentTransformation).changeState(pass);
    // if(!changed && config->expand && this->currentTransformState->getType() == NanoMipsTransform::TransformRelax)
    // {
    //   // Set changed to true to initiate a round of expansion transformations
    //   changed = true;
    //   this->currentTransformState = &expandTransform;
    // }
  }
  return changed;
}

template <class ELFT>
void NanoMips<ELFT>::transform(InputSection *sec) const 
{
  NanoMipsContextProperties &contextProperties = this->currentTransformation.getContextProperties();
  contextProperties.fullNanoMipsISA = NanoMipsAbiFlagsSection<ELFT>::get()->isFullNanoMipsISA(sec);
  auto *obj = sec->getFile<ELFT>();

  // TODO: Don't know if there isn't an object file (and why it isn't there)
  // what to put in as pcrel, for now it is false
  contextProperties.pcrel = obj ? isNanoMipsPcRel<ELFT>(obj) : false;
  const uint32_t bits = config->wordsize * 8;
  uint64_t secAddr = sec->getOutputSection()->addr + sec->outSecOff;
  // Need to do it like this bc at transform we may invalidate the iterator
  // TODO: Relocs are not sorted by offset, check if they should be?
  // TODO: Comdat behaviour?
  // TODO: GP setup from gold is different than lld, probably should change it
  // also probably should make ElfSym::nanoMipsGp - as it represents this better
  for(uint32_t relNum = 0; relNum < sec->relocations.size(); relNum++)
  {
    Relocation &reloc = sec->relocations[relNum];

    if(reloc.type == R_NANOMIPS_ALIGN)
    {
      this->align(sec, reloc, relNum);
      continue;
    }
    // TODO: Check if section should be compressed when returning value
    uint64_t addrLoc = secAddr + reloc.offset;
    uint64_t valueToRelocate = llvm::SignExtend64(sec->getRelocTargetVA(sec->file, reloc.type, reloc.addend, addrLoc, *reloc.sym, reloc.expr), bits);
    const NanoMipsRelocProperty *relocProp =  relocPropertyTable.getRelocProperty(reloc.type);
    if(!relocProp) continue;

    uint32_t instSize = relocProp->getInstSize();
    // 48 bit instruction reloc offsets point to 32 bit imm/off not to the beginning of ins~
    uint32_t relocOffset = reloc.offset - (instSize == 6 ? 2 : 0);
    uint64_t insn = readInsn<ELFT::TargetEndianness>(sec->data(), relocOffset, instSize);

    uint64_t insMask = relocProp->getMask();
    LLVM_DEBUG(
      llvm::dbgs() << "Reloc property: " << relocProp->getName() << "\n";
      llvm::dbgs() << "\tInsMask: 0x" << utohexstr(insMask) << "\n";
      llvm::dbgs() << "Instruction Read: 0x" << utohexstr(insn) << "\n";
    );

    const NanoMipsInsProperty *insProperty = this->currentTransformation.getInsProperty(insn, insMask, reloc.type, sec);
    if(!insProperty) continue;

    LLVM_DEBUG(
      llvm::dbgs() << "InsProperty: " << insProperty->toString() << "\n";
    );

    // if is forced length
    // TODO: Should check if R_NANOMIPS_INSN32 should allow expansion, or INSN16
    // Note: Will skip this step, don't know how to generate FIXED,INSN32 or INSN16 relocs
    // TODO: Find out how to generate these relocations

    // Note: Will skip symbol calculation as well, we calculate them through getRelocTargetVA 
    // TODO: Return to this later, and see if somethings need to be fixed

    // TODO: Undef weak symbols
    const NanoMipsTransformTemplate *transformTemplate = this->currentTransformation.getTransformTemplate(insProperty, reloc, valueToRelocate, insn, sec);

    if(!transformTemplate) continue;
    LLVM_DEBUG( 
      llvm::dbgs() << "Chosen transform template:\n" << transformTemplate->toString() << "\n";
    );

    // TODO: gold creates a new input section, check if it is needed?

    // Bytes to remove/add
    int32_t delta = transformTemplate->getSizeOfTransform() - instSize;
    if(delta != 0)
      this->currentTransformation.updateSectionContent(sec, relocOffset + instSize, delta);

    // Transform
    // Note: Reloc may be invalidated, but we don't need it from this point on
    // To restore it use its relNum, not the one after transform as it changes
    this->currentTransformation.transform(&reloc, transformTemplate, insProperty, relocProp, sec, insn, relNum);

    auto &newInsns = this->currentTransformation.getNewInsns();
    for(auto &newInsn : newInsns)
    {
      writeInsn<ELFT::TargetEndianness>(newInsn.insn, sec->data(), newInsn.offset, newInsn.size);
    }
    newInsns.clear();
    // Finalize content?
  }
  return;
}

template <class ELFT>
void NanoMips<ELFT>::initTransformAuxInfo() const
{
  // Storage is used in higher llvms
  SmallVector<InputSection *, 0> storage;
  for(OutputSection *osec: outputSections)
  {
    if((osec->flags & (SHF_EXECINSTR | SHF_ALLOC)) != (SHF_EXECINSTR | SHF_ALLOC) ||
          !(osec->type & SHT_PROGBITS))
          continue;
    
    for(InputSection *sec : getInputSections(osec))
    {
      if(!this->safeToModify(sec) || sec->numRelocations == 0) continue;
      sec->nanoMipsRelaxAux = make<NanoMipsRelaxAux>();
      sec->nanoMipsRelaxAux->prevBytesDropped = sec->bytesDropped;
      sec->bytesDropped = 0;
    }
  }
}

template <class ELFT>
void NanoMips<ELFT>::align(InputSection *sec, Relocation &reloc, uint32_t relNum) const
{
  // TODO: Maybe change this so we get them from InsProperties somehow
  // not hardcode like this
  // TODO: Find out how to specify fill size and max, as fill size is always 1 byte,
  // and max cannot even be specified for align
  const uint32_t nop32 = 0x8000c000;
  const uint32_t nop16 = 0x9008;

  uint64_t align = 1 << reloc.sym->getVA();
  uint64_t addr = sec->getOutputSection()->addr + sec->outSecOff + reloc.offset;
  // Note: the reinterpret cast is safe here, as in alignAddr function it is also
  // used to change the pointer to an unsigned long
  uint64_t newAddr = alignAddr(reinterpret_cast<void *>(addr), Align(align));

  uint64_t newPadding = newAddr - addr;
  uint64_t oldPadding = reloc.sym->getSize();
  
  uint64_t fill = nop16;
  uint64_t max = ELFT::Is64Bits ? (uint64_t)(0) - 1 : (uint32_t)(0) - 1;
  size_t fillSize = 2;

  for(uint32_t i = relNum + 1; i < sec->relocations.size(); i++)
  {
    Relocation &r = sec->relocations[i];
    if(r.offset != reloc.offset)
      break;

    if(r.type == R_NANOMIPS_FILL)
    {
      fill = r.sym->getVA();
      fillSize = cast<Defined>(r.sym)->size;
    }
    else if(r.type == R_NANOMIPS_MAX)
    {
      max = r.sym->getVA();
    }
  }

  // Set the padding to 0, if the padding bytes exceed max bytes
  if(newPadding > max)
    newPadding = 0;

  // Equal paddings, mean nothing should change, so return
  if(newPadding == oldPadding)
    return;

  int64_t count = (int64_t)(newPadding - oldPadding);

  // Check if we are cutting nop32 on half, then we need
  // to replace it with nop16 instruction
  if(count < 0 && newPadding >= 2)
  {
    uint64_t insn = readInsn<ELFT::TargetEndianness>(sec->data(), reloc.offset + newPadding - 2, 4);
    if(insn == nop32)
    {
      writeInsn<ELFT::TargetEndianness>(nop16, sec->data(), reloc.offset + newPadding - 2, 2);
      LLVM_DEBUG(
      llvm::dbgs() << "nop[32] is replaced with nop[16] due to new alignment on offset " << reloc.offset + newPadding - 2 
                << " in section " << sec->name << " from obj " << (sec->file ? sec->file->getName() : "None") << "\n";
      );
    }
  }
  
  this->currentTransformation.updateSectionContent(sec, reloc.offset + oldPadding, count, true);
  // Update size of symbol, cast is used because we are sure this symbol is defined
  // if it is not defined then there is an error in code
  cast<Defined>(reloc.sym)->size = newPadding;

  // Add padding
  if(count > 0)
  {
    if(fillSize > (uint64_t)count)
    {
      fill = nop16;
      fillSize = 2;
    }

    for(int i = 0; i < count; i += fillSize)
    {
      // This shouldn't really happen among instructions
      if(fillSize == 1)
        write8(const_cast<uint8_t *>(sec->data().begin()) + reloc.offset + oldPadding + i, fill);
      else
        writeInsn<ELFT::TargetEndianness>(fill, sec->data(), reloc.offset + oldPadding + i, fillSize);
    }
  }
}

template <class ELFT>
void NanoMips<ELFT>::finalizeRelaxations() const {
  // Return previous bytesDropped values
  // and change array ref sizes
   // Storage is used in higher llvms
  SmallVector<InputSection *, 0> storage;
  for(OutputSection *osec: outputSections)
  {
    if((osec->flags & (SHF_EXECINSTR | SHF_ALLOC)) != (SHF_EXECINSTR | SHF_ALLOC) ||
          !(osec->type & SHT_PROGBITS))
          continue;
    
    for(InputSection *sec : getInputSections(osec))
    {
      if(!this->safeToModify(sec) || sec->numRelocations == 0) continue;
      // This means we have more bytes than needed, shorten the array ref
      if(sec->bytesDropped)
      {
        sec->setRawData(sec->data().drop_back(sec->bytesDropped));
      }
      sec->bytesDropped = sec->nanoMipsRelaxAux->prevBytesDropped;
    }
  }
}

// bool NanoMips::relax(InputSection *sec) const {

//   auto &relocations = sec->relocations;
//   bool changed = false;
//   const uint32_t bits = config->wordsize * 8;
//   uint64_t secAddr = sec->getOutputSection()->addr + sec->outSecOff;
//   for(auto &reloc : relocations)
//   {
//     // TODO: Check if section should be compressed when returning value
//     ArrayRef<uint8_t> oldContent = sec->data();
//     uint64_t oldSize = sec->getSize();
//     uint64_t addrLoc = secAddr + reloc.offset;
//     uint64_t valueToRelocate = SignExtend64(sec->getRelocTargetVA(sec->file, reloc.type, reloc.addend, addrLoc, *reloc.sym, reloc.expr), bits);
//     const NanoMipsRelocProperty *relocProp =  relocPropertyTable.getRelocProperty(reloc.type);
//     if(relocProp)
//     {
//       llvm::outs() << relocProp->getName() << "\n";
//     }
//     switch(reloc.type)
//     {
//       case R_NANOMIPS_PC14_S1:
//       {
//         uint64_t insn = support::endian::read32le(&oldContent[reloc.offset]);
//         // For more natural working with instruction
//         insn = bswap(insn);
//         // Check if it is a beqc instruction
//         // TODO: Make this prettier
//         if((insn >> 26) != 0x22 || ((insn >> 14) & 0x3) != 0x00) break; 
//         // 4 is the size of instruction
//         valueToRelocate -= 4;
//         // TODO: Make extract bits function
//         uint32_t srcReg =  (insn >> 16) & 0x1f;
//         uint32_t dstReg = (insn >> 21) & 0x1f;
//         // TODO: Check if we should change the value for relocation, as we are going to generate a 2 byte instruciton
//         // FIXME: srcReg and dstReg not good, shouldn't only be less than 7
//         if(valueToRelocate != 0 && srcReg != 0 && srcReg <= 7 && dstReg <= 7 && srcReg != dstReg && isUInt<5>(valueToRelocate))
//         {
//           // New instruction is smaller for 2 bytes
//           // TODO: Used make_unique here, as I know how to use it, should change to some sort of allocator
//           // like the BumpPointerAllocator that is being used in InputSection, this is also very inefficient 
//           // as there is a lot of unecessary copying but it is used to check if this will work
//           auto newContentUnique = std::make_unique<uint8_t>((oldSize - 2) * sizeof(uint8_t));
//           uint8_t *newContent = newContentUnique.get();
//           uint64_t newSize = oldSize - 2;
//           // Swap src and dest if they don't conform to the abi (rs3<rt3)
//           if(srcReg > dstReg)
//           {
//             uint32_t tmp = srcReg;
//             srcReg = dstReg;
//             dstReg = tmp;
//           }
//           uint32_t newIns = 0x36 << 10 | (dstReg & 7) << 7 | (srcReg & 7) << 4 | 0;
//           uint32_t oldI = 0;
//           uint32_t newI = 0;
//           while(oldI != oldSize)
//           {
//             if(oldI != reloc.offset)
//             {
//               newContent[newI] = oldContent[oldI];
//               newI++;
//               oldI++;
//             }
//             else{
//               support::endian::write16le(newContent + newI, newIns);
//               newI += 2;
//               oldI += 4;
//             }
//           }
//           newI = 0;
//           while(newI != newSize)
//           {
//             const_cast<uint8_t *>(oldContent.data())[newI] = newContent[newI];
//             newI++;
//           }
//           // Inefficient as well, change relocs to have good offset
//           for(auto &reloc2 : relocations)
//           {
//             if(reloc2.offset > reloc.offset)
//               reloc2.offset -= 2;
//           }

//           // Inefficient as well, adjust symbols
//           // TODO: Change size of function
//           if(sec->file)
//           {
//             for(auto *sym: sec->file->symbols)
//             {
//               if(isa<Defined>(sym))
//               {
//                 auto dSym = cast<Defined>(sym);
//                 if(sym->getOutputSection() && sym->getOutputSection()->sectionIndex == sec->getOutputSection()->sectionIndex)
//                 {
//                   if(dSym->value > reloc.offset)
//                   {
//                     dSym->value -= 2;
//                   }
//                 }
//               }

//             }
//           }
//           reloc.type = R_NANOMIPS_PC4_S1;
//           sec->drop_back(oldSize - newSize);
//           changed = true;
//         }
//         else if(srcReg == 0 && isInt<8>(valueToRelocate))
//         {
//           // TODO
//           llvm::outs() << "Can relocate to beqz version, but not implemented yet!\n";
//         }
//         break;
//       }
//       default:
//         break;
//     }
//   }
//   return changed;
// }

// TODO: Check if we can mix these into relax and expand into one function
// bool NanoMips::expand(InputSection *sec) const {
//   llvm::outs() << "expand called\n";
//   auto &relocations = sec->relocations;
//   bool changed = false;
//   const uint32_t bits = config->wordsize * 8;
//   const uint64_t secAddr = sec->getOutputSection()->addr + sec->outSecOff;
//   for(auto &reloc: relocations)
//   {
//     // TODO: Check if section should be compressed when returning value
//     ArrayRef<uint8_t> oldContent = sec->data();
//     uint64_t oldSize = sec->getSize();
//     uint64_t addrLoc = secAddr + reloc.offset;
//     uint64_t valueToRelocate = SignExtend64(sec->getRelocTargetVA(sec->file, reloc.type, reloc.addend, addrLoc, *reloc.sym, reloc.expr), bits);
//     switch(reloc.type)
//     {
//       case R_NANOMIPS_PC21_S1:
//       {
//         llvm::outs() << "Expansion of R_NANOMIPS_PC21_S1\n";
//         valueToRelocate -= 4;
//         uint32_t insn = support::endian::read32le(&oldContent[reloc.offset]);
//         insn = bswap(insn);
//         // lapc
//         llvm::outs() << insn << "\n";
//         if((insn >> 26) != 0x1) break;
//         llvm::outs() << "Expansion of lapc\n";
//         uint32_t dstReg = (insn >> 21) & 0x1f;

//         if((valueToRelocate & 0x1) == 0 && !isInt<22>(valueToRelocate))
//         {
//           auto newContentUnique = std::make_unique<uint8_t>((oldSize + 2) * sizeof(uint8_t));
//           uint8_t *newContent = newContentUnique.get();
//           uint64_t newSize = oldSize + 2;
//           uint64_t newIns = 0x18UL << 42 | static_cast<uint64_t>(dstReg) << 37 | 0x3UL << 32 | 0x0UL;
//           newIns = bswap48(newIns);
//           uint32_t oldI = 0;
//           uint32_t newI = 0;
//           llvm::outs() << newIns << "\n";
//           while(oldI != oldSize)
//           {
//             if(oldI != reloc.offset)
//             {
//               newContent[newI] = oldContent[oldI];
//               newI++;
//               oldI++;
//             }
//             else{
//               support::endian::write32le(newContent + newI, newIns & 0xffffffffL);
//               support::endian::write16le(newContent + newI + 4, newIns >> 32);
//               newI += 6;
//               oldI += 4;
//             }
//           }

//           llvm::outs() << sec->bytesDropped << "\n";
//           if(sec->bytesDropped < 2)
//           {
//             sec->increaseSizeOfSection(2 - sec->bytesDropped);
//           }
//           oldContent = sec->data();
//           sec->push_back(newSize - oldSize);
//           newI = 0;
//           while(newI != newSize)
//           {
//             const_cast<uint8_t *>(oldContent.data())[newI] = newContent[newI];
//             llvm::outs() << (uint32_t)newContent[newI] << "\n";
//             newI++;
//           }

//           for(auto &reloc2: relocations)
//           {
//             if(reloc2.offset > reloc.offset)
//               reloc2.offset += 2;
//           }

//           if(sec->file)
//           {
//             for(auto *sym : sec->file->symbols)
//             {
//               if(isa<Defined>(sym))
//               {
//                 auto dSym = cast<Defined>(sym);
//                 if(sym->getOutputSection() && sym->getOutputSection()->sectionIndex == sec->getOutputSection()->sectionIndex)
//                 {
//                   if(dSym->value > reloc.offset)
//                     dSym->value += 2;
//                 }
//               }
//             }
//           }

//           reloc.type = R_NANOMIPS_PC_I32;
//           reloc.offset += 2;
//           // TODO: See what to do about the assert in this function, we need to see how to change the size of sections
//           changed = true;
//         }
//       }
//         break;
//       default:
//         break;
//     }
//   }
//   return changed;
// }



template TargetInfo *elf::getNanoMipsTargetInfo<ELF32LE>();
template TargetInfo *elf::getNanoMipsTargetInfo<ELF32BE>();
template TargetInfo *elf::getNanoMipsTargetInfo<ELF64LE>();
template TargetInfo *elf::getNanoMipsTargetInfo<ELF64BE>();

uint64_t elf::getNanoMipsNegCompositeRelDataAlloc(Relocation *&it, Relocation *&end, uint8_t *bufLoc, uint8_t *buf, InputSectionBase *sec, const InputFile *file, uint64_t addrLoc)
{
  const Relocation &rel = *it++;
  const unsigned bits = config->wordsize * 8;
  RelType type = rel.type;
  Symbol &sym = *rel.sym;
  assert(type == R_NANOMIPS_NEG && "First relocation type for R_NANOMIPS_NEG_COMPOSITE must be R_NANOMIPS_NEG");
  assert(it != end && "R_NANOMIPS_NEG_COMPOSITE is composed of more than one relocation");
  
  const uint64_t targetVA = llvm::SignExtend64(sec->getRelocTargetVA(
    file, type, rel.addend, addrLoc, sym, rel.expr
    ), 
    bits);
  const Relocation &next1 = *it;
  uint64_t offset1 = next1.offset;
  uint8_t *bufLoc1 = buf + offset1;
  RelType type1 = next1.type;
  Symbol &sym1 = *next1.sym;
  if(bufLoc != bufLoc1)
  {
      message("Incorrect logic for negative and shift");
      exit(6);
  }
  const uint64_t targetVA1 = llvm::SignExtend64(sec->getRelocTargetVA(
    file, type1, next1.addend, addrLoc, sym1, next1.expr
    ), 
    bits);
  if(type1 == R_NANOMIPS_ASHIFTR_1)
  {
    it++;
    assert(it != end && "R_NANOMIPS_NEG_COMPOSITE with R_NANOMIPS_ASHIFTR_1 consists of one more relocation");
    const Relocation &next2 = *it;
    uint64_t offset2 = next2.offset;
    uint8_t *bufLoc2 = buf + offset2;
    RelType type2 = next2.type;
    Symbol &sym2 = *next2.sym;
    if(bufLoc != bufLoc2)
    {
      message("Incorrect logic for negative and shift");
      exit(6);
    }

    const uint64_t targetVA2 = llvm::SignExtend64(sec->getRelocTargetVA(
      file, type2, next2.addend, addrLoc, sym2, next2.expr
      ), bits);
    uint64_t data = (targetVA1 + targetVA) >> 1; 
    if(type2 == R_NANOMIPS_SIGNED_16 || type2 == R_NANOMIPS_SIGNED_8)
      data = llvm::SignExtend64(data, bits);
    data += targetVA2;
    return data;
  }
  else
  {
    uint64_t data = targetVA1 + targetVA;
    if(type1 == R_NANOMIPS_SIGNED_16 || type1 == R_NANOMIPS_SIGNED_8)
      data = llvm::SignExtend64(data, bits);
    return data;
  }


}

template<class ELFT, class RelTy> 
uint64_t elf::getNanoMipsNegCompositeRelDataNonAlloc(typename ArrayRef<RelTy>::iterator &it, typename ArrayRef<RelTy>::iterator &end, uint8_t *bufLoc, uint8_t *buf, InputSectionBase *sec, const InputFile *file, uint64_t addrLoc, const TargetInfo *target)
{
  const RelTy &rel = *it++;
  const unsigned bits = config->wordsize * 8;
  RelType type = rel.getType(config->isMips64EL);
  Symbol &sym = sec->getFile<ELFT>()->getRelocTargetSym(rel);
  assert(type == llvm::ELF::R_NANOMIPS_NEG && "First relocation type for R_NANOMIPS_NEG_COMPOSITE must be R_NANOMIPS_NEG");
  assert(it != end && "R_NANOMIPS_NEG_COMPOSITE is composed of more than one relocation");
  
  const uint64_t targetVA = llvm::SignExtend64(sec->getRelocTargetVA(
    file, type, getAddend<ELFT>(rel), addrLoc, sym, target->getRelExpr(type, sym, bufLoc)
    ), 
    bits);
  const RelTy &next1 = *it;
  uint64_t offset1 = next1.r_offset;
  uint8_t *bufLoc1 = buf + offset1;
  RelType type1 = next1.getType(config->isMips64EL);
  Symbol &sym1 = sec->getFile<ELFT>()->getRelocTargetSym(next1);
  if(bufLoc != bufLoc1)
  {
      message("Incorrect logic for negative and shift");
      exit(6);
  }
  const uint64_t targetVA1 = llvm::SignExtend64(sec->getRelocTargetVA(
    file, type1, getAddend<ELFT>(next1), addrLoc, sym1, target->getRelExpr(type1, sym1, bufLoc1)
    ), 
    bits);
  if(type1 == llvm::ELF::R_NANOMIPS_ASHIFTR_1)
  {
    it++;
    assert(it != end && "R_NANOMIPS_NEG_COMPOSITE with R_NANOMIPS_ASHIFTR_1 consists of one more relocation");
    const RelTy &next2 = *it;
    uint64_t offset2 = next2.r_offset;
    uint8_t *bufLoc2 = buf + offset2;
    RelType type2 = next2.getType(config->isMips64EL);
    Symbol &sym2 = sec->getFile<ELFT>()->getRelocTargetSym(next2);
    if(bufLoc != bufLoc2)
    {
      message("Incorrect logic for negative and shift");
      exit(6);
    }

    const uint64_t targetVA2 = llvm::SignExtend64(sec->getRelocTargetVA(
      file, type2, getAddend<ELFT>(next2), addrLoc, sym2, target->getRelExpr(type2, sym2, bufLoc2)
      ), bits);
    uint64_t data = (targetVA1 + targetVA) >> 1; 
    if(type2 == llvm::ELF::R_NANOMIPS_SIGNED_16 || type2 == llvm::ELF::R_NANOMIPS_SIGNED_8)
      data = llvm::SignExtend64(data, bits);
    data += targetVA2;
    return data;
  }
  else
  {
    uint64_t data = targetVA1 + targetVA;
    if(type1 == llvm::ELF::R_NANOMIPS_SIGNED_16 || type1 == llvm::ELF::R_NANOMIPS_SIGNED_8)
      data = llvm::SignExtend64(data, bits);
    return data;
  }


}

template uint64_t elf::getNanoMipsNegCompositeRelDataNonAlloc<ELF32LE, ELF32LE::Rel>(ArrayRef<ELF32LE::Rel>::iterator &, ArrayRef<ELF32LE::Rel>::iterator &, uint8_t *, uint8_t *, InputSectionBase *, const InputFile *, uint64_t, const TargetInfo *);
template uint64_t elf::getNanoMipsNegCompositeRelDataNonAlloc<ELF32LE, ELF32LE::Rela>(ArrayRef<ELF32LE::Rela>::iterator &, ArrayRef<ELF32LE::Rela>::iterator &, uint8_t *, uint8_t *, InputSectionBase *, const InputFile *, uint64_t, const TargetInfo *);
template uint64_t elf::getNanoMipsNegCompositeRelDataNonAlloc<ELF32BE, ELF32BE::Rel>(ArrayRef<ELF32BE::Rel>::iterator &, ArrayRef<ELF32BE::Rel>::iterator &, uint8_t *, uint8_t *, InputSectionBase *, const InputFile *, uint64_t, const TargetInfo *);
template uint64_t elf::getNanoMipsNegCompositeRelDataNonAlloc<ELF32BE, ELF32BE::Rela>(ArrayRef<ELF32BE::Rela>::iterator &, ArrayRef<ELF32BE::Rela>::iterator &, uint8_t *, uint8_t *, InputSectionBase *, const InputFile *, uint64_t, const TargetInfo *);
template uint64_t elf::getNanoMipsNegCompositeRelDataNonAlloc<ELF64LE, ELF64LE::Rel>(ArrayRef<ELF64LE::Rel>::iterator &, ArrayRef<ELF64LE::Rel>::iterator &, uint8_t *, uint8_t *, InputSectionBase *, const InputFile *, uint64_t, const TargetInfo *);
template uint64_t elf::getNanoMipsNegCompositeRelDataNonAlloc<ELF64LE, ELF64LE::Rela>(ArrayRef<ELF64LE::Rela>::iterator &, ArrayRef<ELF64LE::Rela>::iterator &, uint8_t *, uint8_t *, InputSectionBase *, const InputFile *, uint64_t, const TargetInfo *);
template uint64_t elf::getNanoMipsNegCompositeRelDataNonAlloc<ELF64BE, ELF64BE::Rel>(ArrayRef<ELF64BE::Rel>::iterator &, ArrayRef<ELF64BE::Rel>::iterator &, uint8_t *, uint8_t *, InputSectionBase *, const InputFile *, uint64_t, const TargetInfo *);
template uint64_t elf::getNanoMipsNegCompositeRelDataNonAlloc<ELF64BE, ELF64BE::Rela>(ArrayRef<ELF64BE::Rela>::iterator &, ArrayRef<ELF64BE::Rela>::iterator &, uint8_t *, uint8_t *, InputSectionBase *, const InputFile *, uint64_t, const TargetInfo *);

