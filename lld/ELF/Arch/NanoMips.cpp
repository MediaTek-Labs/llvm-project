//===- Nanomips.cpp -------------------------------------------------*- C++ -*-===//
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

using namespace llvm::object;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;
using namespace llvm::support::endian;
using namespace llvm;

// use --mllvm with --debug or --debug-only=<name>
#define DEBUG_TYPE "lld-nanomips"


uint32_t bswap(uint32_t data){
  uint32_t lo = ((data & 0xffff) << 16) & ~0xffff;
  uint32_t hi = ((data & ~0xffff) >> 16) & 0xffff;
  return lo | hi;
}

// No need for this anymore we are using only the first 16bit of 48bit instruction
// Swap for 48bit instructions
uint64_t bswap48(uint64_t data){
  uint64_t lo = ((data & 0xffff) << 32);
  uint64_t mid = (data & 0xffff0000);
  uint64_t hi = (data & ~0xffffffffL) >> 32;
  return lo | mid | hi;
}

uint64_t elf::getNanoMipsPage(uint64_t expr) {
  return expr & ~static_cast<uint64_t>(0xFFF);
}

namespace {

  
// TO DO: Support for other endianess, and bit size, now it is Little Endian 32 bit
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
    enum TransformationState {
      NANOMIPS_NONE_STATE,
      NANOMIPS_RELAX_STATE,
      NANOMIPS_EXPAND_STATE
    };
    mutable TransformationState transformationState = NANOMIPS_NONE_STATE;
    NanoMipsRelocPropertyTable relocPropertyTable;
    NanoMipsInsPropertyTable insPropertyTable;

    bool safeToModify(InputSection *sec) const;
    void setAbiFlags() const;

    // relax + expand
    bool transform(InputSection *sec) const;


    // bool relax(InputSection *sec) const;
    // bool expand(InputSection *sec) const;
};
} // namespace

NanoMips::NanoMips(){
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
  );
}

//used for: R_NANOMIPS_HI20, R_NANOMIPS_PC_HI20 and R_NANOMIPS_GPREL_HI20
static void writeValueHi20(uint8_t *loc, uint64_t val) {
  uint32_t instr = read32(loc);
  instr = bswap(instr);
  uint32_t data = (val & ~1) | ((val >> 31) & 1);
  data = (data & ~0xffc) | ((val >> 19) & 0xffc); 
  uint32_t masked = (instr & ~0x1ffffd) | (data & 0x1ffffd);
  masked = bswap(masked);
  write32(loc, masked);
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
static void writePcRel32(uint8_t *loc, uint64_t val, uint8_t bitsSize) {
    uint32_t instr = read32(loc); 
    instr = bswap(instr);
    val = (val & ~1) | ((val >> bitsSize) & 1);
    uint32_t mask = (0xffffffff >> (32 - bitsSize));
    uint32_t data = (instr & ~mask) | (val & mask);
    data = bswap(data);
    write32(loc, data);
}

//used for: R_NANOMIPS_LO12, R_NANOMIPS_GPREL19_S2, R_NANOMIPS_GPREL18, R_NANOMIPS_GPREL17_S1 and R_NANOMIPS_GPREL_LO12
static void writeValue32be(uint8_t *loc, uint64_t val, uint8_t bitsSize,
                       uint8_t shift) {
    uint32_t instr = read32(loc); 
    instr = bswap(instr);
    uint32_t mask = (0xffffffff >> (32 - bitsSize)) << shift;
    uint32_t data = (instr & ~mask) | (val & mask);
    data = bswap(data);
    write32(loc, data);
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

RelExpr NanoMips::getRelExpr(RelType type, const Symbol &s,
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
  case R_NANOMIPS_ALIGN:
    return R_NONE;
  default:
    error(getErrorLocation(loc) + "unknown relocation (" + Twine(type) +
          ") against symbol " + toString(s) + " loc: " + llvm::utohexstr(uint64_t(loc)) + " file name: " + toString(s.file->getName()) );
    return R_NONE;
  }                   
}

void NanoMips::relocate(uint8_t *loc, const Relocation &rel, uint64_t val) const {
  switch (rel.type) {
  case R_NANOMIPS_PC_I32:
    checkInt(loc, val - 4, 32, rel);
    write32(loc, val - 4);
    break;
  case R_NANOMIPS_PC32:
  case R_NANOMIPS_32:
  case R_NANOMIPS_I32:
  case R_NANOMIPS_GPREL_I32:
    checkInt(loc, val, 32, rel);
    write32(loc, val);
    break;
  case R_NANOMIPS_NEG:
  case R_NANOMIPS_ASHIFTR_1: 
    assert(0 && "Incorrect logic for R_NANOMIPS_NEG and R_NANOMIPS_ASHIFTR_1");
    break;
  case R_NANOMIPS_NONE:
  case R_NANOMIPS_FIXED:
  case R_NANOMIPS_ALIGN:
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
    writeValueHi20(loc, val); 
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
    checkIntPcRel(loc, val - 4, 26, rel, *rel.sym);
    writePcRel32(loc, val - 4, 25);
    break;
  case R_NANOMIPS_PC21_S1:
    checkIntPcRel(loc, val - 4, 22, rel, *rel.sym);
    writePcRel32(loc, val - 4, 21);
    break;
  case R_NANOMIPS_PC14_S1:
    checkIntPcRel(loc, val - 4, 15, rel, *rel.sym);
    writePcRel32(loc, val - 4, 14);
    break;
  case R_NANOMIPS_PC11_S1:
    checkIntPcRel(loc, val - 4, 12, rel, *rel.sym);
    writePcRel32(loc, val - 4, 11);
    break;
  case R_NANOMIPS_LO12:
    writeValue32be(loc, val, 12, 0);
    break;
  case R_NANOMIPS_GPREL19_S2:
    checkUInt(loc, val, 21, rel);
    writeValue32be(loc, val, 19, 2);
    break;
  case R_NANOMIPS_GPREL18:
    checkUInt(loc, val, 18, rel);
    writeValue32be(loc, val, 18, 0);
    break;
  case R_NANOMIPS_GPREL17_S1:
    checkUInt(loc, val, 18, rel);
    writeValue32be(loc, val, 17, 1);
    break;
  case R_NANOMIPS_GPREL_LO12:
    checkInt(loc, val, 12, rel);
    writeValue32be(loc, val, 12, 0);
    break;
  default:
    llvm_unreachable("unknown relocation");
    break;
  }
}

TargetInfo *elf::getNanoMipsTargetInfo() {
    static NanoMips t;
    return &t;
}

bool NanoMips::mayRelax() const
{
  // TODO: When the finalize-relocs option is added, change this expression
  // also goes for sort-by-reference option
  return (!config->relocatable && (config->relax || config->expand));
}

bool NanoMips::safeToModify(InputSection *sec) const
{
  bool modifiable = false;
  if(auto *obj = sec->getFile<ELF32LE>())
  {
    modifiable = (obj->getObj().getHeader().e_flags & EF_NANOMIPS_LINKRELAX) != 0;
  }
  return modifiable;
}

bool NanoMips::relaxOnce(int pass) const
{
  // TODO: isFullNanoMipsISA is not compatible with gold's checking of nmf, as it is checked
  // per obj file, here we check the output one.
  llvm::outs() << "is full nanoMIPS ISA: "  << NanoMipsAbiFlagsSection<ELF32LE>::get()->isFullNanoMipsISA() << "\n";
  bool changed = false;
  if(this->mayRelax())
  {
    if(pass == 0)
    {
      if(config->relax) this->transformationState = NANOMIPS_RELAX_STATE;
      else if(config->expand) this->transformationState = NANOMIPS_EXPAND_STATE;
    }
    for(OutputSection *osec : outputSections)
    {
      if((osec->flags & (SHF_EXECINSTR | SHF_ALLOC)) != (SHF_EXECINSTR | SHF_ALLOC) ||
          !(osec->type & SHT_PROGBITS))
          continue;
      for(InputSection *sec : getInputSections(osec))
      {
        if(!this->safeToModify(sec)) continue;

        if((this->transformationState == NANOMIPS_RELAX_STATE || this->transformationState == NANOMIPS_EXPAND_STATE)  && sec->numRelocations)
         changed = this->transform(sec) || changed;

      }
    }
    if(!changed && config->expand && this->transformationState == NANOMIPS_RELAX_STATE)
    {
      changed = true;
      this->transformationState = NANOMIPS_EXPAND_STATE;
    }
  }
  return changed;
}

bool NanoMips::transform(InputSection *sec) const 
{

  auto &relocs = sec->relocations;
  bool changed = false;
  const uint32_t bits = config->wordsize * 8;
  uint64_t secAddr = sec->getOutputSection()->addr + sec->outSecOff;
  for(auto &reloc: relocs)
  {
    // TODO: Check if section should be compressed when returning value
    ArrayRef<uint8_t> oldContent = sec->data();
    uint64_t oldSize = sec->getSize();
    uint64_t addrLoc = secAddr + reloc.offset;
    uint64_t valueToRelocate = llvm::SignExtend64(sec->getRelocTargetVA(sec->file, reloc.type, reloc.addend, addrLoc, *reloc.sym, reloc.expr), bits);
    const NanoMipsRelocProperty *relocProp =  relocPropertyTable.getRelocProperty(reloc.type);
    if(!relocProp) continue;

    uint64_t insn = 0;
    uint32_t instSize = relocProp->getInstSize();
    if(instSize == 4)
    {
      insn = read32le(&oldContent[reloc.offset]);
      insn = bswap(insn);
    } 
    else if(instSize == 6)
      insn = read16le(&oldContent[reloc.offset - 2]);
    else if(instSize == 2)
      insn = read16le(&oldContent[reloc.offset]);
    else continue;

    uint64_t insMask = relocProp->getMask();
    LLVM_DEBUG(
      llvm::dbgs() << "Reloc property: " << relocProp->getName() << "\n";
      llvm::dbgs() << "\tInsMask: " << utohexstr(insMask) << "\n";
    );
  }
  return changed;
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

