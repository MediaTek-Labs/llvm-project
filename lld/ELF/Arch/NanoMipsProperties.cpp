//===- NanoMipsProperties.cpp
//-----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "NanoMipsProperties.h"
#include "Config.h"
#include "OutputSections.h"
#include "SymbolTable.h"
#include "Symbols.h"
#include "SyntheticSections.h"
#include "lld/Common/Memory.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/ELFTypes.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/raw_ostream.h"
#include <sstream>

#include "Target.h"

#define DEBUG_TYPE "lld-nanomips"
using namespace lld;
using namespace lld::elf;
using namespace llvm;
using namespace llvm::ELF;
using namespace llvm::object;
using namespace llvm::support::endian;

// Function templates for insert, extract, convert and isValid Reg

enum NanoMipsRegisterMapping { RM_NANOMIPS_TREG, RM_NANOMIPS_SREG };

template <uint32_t Pos, uint32_t Size, NanoMipsRegisterMapping REG>
uint64_t insertReg(uint32_t treg, uint32_t sreg, uint64_t data) {
  uint32_t reg;
  // C++ 17
  // if constexpr(REG == RM_NANOMIPS_TREG) reg = treg;
  // else reg = sreg;

  if (REG == RM_NANOMIPS_TREG)
    reg = treg;
  else
    reg = sreg;

  return data | ((reg & ((1U << Size) - 1)) << Pos);
}

template <uint32_t Pos, uint32_t Size> uint32_t extractReg(uint64_t data) {
  return (data >> Pos) & ((1U << Size) - 1);
}

uint32_t moveBalcTReg(uint64_t data) {
  uint32_t reg = ((data >> 21) & 0x7) | ((data >> 22) & 0x8);
  static uint32_t gpr4ZeroMap[] = {8,  9,  10, 0,  4,  5,  6,  7,
                                   16, 17, 18, 19, 20, 21, 22, 23};
  return gpr4ZeroMap[reg];
}

// We'll call it SReg even though it is a DReg bc every ins has a TReg and a
// SReg in ins properties
uint32_t moveBalcSReg(uint64_t data) { return ((data >> 24) & 0x1) + 4; }

// Convert 3-bit to 5-bit reg
uint32_t convertReg(uint32_t reg) {
  static uint32_t gpr3Map[] = {16, 17, 18, 19, 4, 5, 6, 7};
  assert(reg < sizeof(gpr3Map) / sizeof(gpr3Map[0]) && "Invalid 3bit reg");
  return gpr3Map[reg];
}

// Convert 3-bit store src reg to 5-bit reg
uint32_t convertStoreSrcReg(uint32_t reg) {
  static uint32_t gpr3StoreSrcMap[] = {0, 17, 18, 19, 4, 5, 6, 7};
  assert(reg < sizeof(gpr3StoreSrcMap) / sizeof(gpr3StoreSrcMap[0]) &&
         "Invalid 3bit reg");
  return gpr3StoreSrcMap[reg];
}

// Check if a 5-bit reg can be converted to a 3-bit one
bool isRegValid(uint32_t reg) {
  return ((4 <= reg && reg <= 7) || (16 <= reg && reg <= 19));
}

// Check if a 5-bit store source register can be converted to a 3-bit one
bool isStoreSrcRegValid(uint32_t reg) {
  return (reg == 0 || (4 <= reg && reg <= 7) || (17 <= reg && reg <= 19));
}

// Functions for checking if some instructions need to be fixed

static bool valueNeedsHw110880Fix(uint64_t val) {
  if (!config->nanoMipsFixHw110880)
    return false;
  return ((val & 0x50016400) == 0x50012400) &&
         (((val >> 24) & 0x7) == 0x1 || ((val >> 24) & 0x2) == 0x2);
}

// Return rt and u[11:3] fields from nanoMIPS save/restore instruction

uint32_t extractSaveResFields(uint64_t data) {
  return (((data >> 3) & 0x1ff) | ((data >> 12) & 0x3e00));
}

// Insert rt and u[11:3] fields in nanoMIPS save/restore instruction
// sreg contains fields
uint64_t insertSaveResFields(uint32_t treg, uint32_t sreg, uint64_t data) {
  uint32_t u = ((sreg & 0x1ff) << 3);
  uint32_t rt = ((sreg & 0x3e00) << 12);
  return (data | rt | u);
}

// Insert rt and u[7:4] fields in nanoMIPS save[16]/restore.jrc[16] instruction
// sreg contains fields
uint64_t insertSaveRes16Fields(uint32_t treg, uint32_t sreg, uint64_t data) {
  uint32_t u = ((sreg & 0x1ff) << 3);
  uint32_t rt = ((sreg & 0x3e00) >> 9);
  uint32_t rt1 = (rt == 30) ? 0 : (1U << 9);
  return (data | rt1 | u);
}

// NanoMipsRelocPropertyTable

NanoMipsRelocPropertyTable::NanoMipsRelocPropertyTable() {
  for (uint32_t i = 0; i < RelocPropertyTableSize; i++)
    table[i] = nullptr;
#undef RELOC_PROPERTY
#define RELOC_PROPERTY(relocName, instSize, bitsToRelocate, mask)              \
  {                                                                            \
    RelType type = R_NANOMIPS_##relocName;                                     \
    assert(type < RelocPropertyTableSize &&                                    \
           "Relocations mustn't be larger than 256");                          \
    this->table[type] = new NanoMipsRelocProperty(                             \
        "R_NANOMIPS_" #relocName, instSize / 8, bitsToRelocate, mask);         \
  }
#include "NanoMipsRelocInsProperty.inc"
#undef RELOC_PROPERTY
}

NanoMipsRelocPropertyTable::~NanoMipsRelocPropertyTable() {
  llvm::for_each(table, [](auto &elem) {
    delete elem;
    elem = nullptr;
  });
}

std::string NanoMipsRelocPropertyTable::toString() const {
  std::string tmp;
  raw_string_ostream SS(tmp);
  for (uint32_t i = 0; i < RelocPropertyTableSize; i++) {
    if (table[i]) {
      SS << i << "\t" << table[i]->toString() << "\n";
    }
  }
  return SS.str();
}

// NanoMipsInsTemplate

std::string NanoMipsInsTemplate::toString() const {
  std::string tmp;
  raw_string_ostream SS(tmp);
  SS << name << ", " << utohexstr(data) << ", " << reloc << ", " << size << ", "
     << (insertTReg != nullptr ? 1 : 0) << ", "
     << (insertSReg != nullptr ? 1 : 0);
  return SS.str();
}

uint64_t NanoMipsInsTemplate::getInstruction(uint32_t tReg,
                                             uint32_t sReg) const {
  uint64_t insData = data;
  if (insertTReg != nullptr)
    insData = insertTReg(tReg, sReg, insData);
  if (insertSReg != nullptr)
    insData = insertSReg(tReg, sReg, insData);
  return insData;
}

// NanoMipsTransformTemplate

std::string NanoMipsTransformTemplate::toString() const {
  std::string tmp;
  raw_string_ostream SS(tmp);
  SS << "\t\t\tIns count: " << insCount << "\n";
  for (uint32_t i = 0; i < insCount; i++) {
    SS << "\t\t\t\t" << insns[i].toString() << "\n";
  }

  SS << "\t\t\tReloc count: " << relocCount << "\n";
  for (uint32_t i = 0; i < relocCount; i++) {
    SS << "\t\t\t\t" << relocs[i] << "\n";
  }

  SS << "\t\t\tTotal size: " << totalSizeOfTransform << "\n";
  return SS.str();
}

// NanoMipsRelocProperty

std::string NanoMipsRelocProperty::toString() const {
  std::string tmp;
  raw_string_ostream SS(tmp);
  SS << name << "\t" << bitsToRelocate << "\t" << instSize << "\t"
     << utohexstr(mask);
  return SS.str();
}

// NanoMipsInsProperty

void NanoMipsInsProperty::addTransform(
    const NanoMipsTransformTemplate *transformTemplate,
    NanoMipsTransformType type, const RelType *relocs, uint32_t relocCount) {
  // No checks implemented if everything is good
  for (uint32_t i = 0; i < relocCount; i++)
    this->relocs.insert(relocs[i]);

  transformationMap[type] = transformTemplate;
}

std::string NanoMipsInsProperty::toString() const {
  std::string tmp;
  raw_string_ostream SS(tmp);
  SS << name << ", extractTReg: " << (extractTReg != nullptr ? 1 : 0)
     << ", extractSReg: " << (extractSReg != nullptr ? 1 : 0)
     << ", convertTReg: " << (convertTReg != nullptr ? 1 : 0)
     << ", convertSReg: " << (convertSReg != nullptr ? 1 : 0)
     << ", isValidTReg: " << (isValidTReg != nullptr ? 1 : 0)
     << ", isValidSReg: " << (isValidSReg != nullptr ? 1 : 0);

  // SS << "\n\t\tTransforms:";
  // for(auto it = transformationMap.begin(); it != transformationMap.end();
  // it++)
  // {
  //   uint32_t type = it->first;
  //   const NanoMipsTransformTemplate *transformTemplate = it->second;
  //   SS << "\n\t\t\t" << type << "\n" << transformTemplate->toString();
  // }

  // SS << "\n\t\tRelocs:";
  // for(RelType rel : relocs)
  // {
  //   SS << "\n\t\t\t" << rel << "\n";
  // }
  return SS.str();
}

NanoMipsInsProperty::~NanoMipsInsProperty() {
  // C++ 14
  llvm::for_each(transformationMap, [](auto &pair) {
    delete pair.second;
    pair.second = nullptr;
  });
  // for(auto it = transformationMap.begin(); it != transformationMap.end();
  // it++)
  // {
  //   delete it->second;
  //   it->second = nullptr;
  // }
}

// NanoMipsInsPropertyTable

NanoMipsInsPropertyTable::NanoMipsInsPropertyTable(){
#undef EXTRACT_REG
#undef INS_PROPERTY
#define EXTRACT_REG(Pos, Size) extractReg<Pos, Size>
#define INS_PROPERTY(name, opcode, extractTReg, convertTReg, isValidTReg,      \
                     extractSReg, convertSReg, isValidSReg)                    \
  {                                                                            \
    NanoMipsInsProperty *insProp =                                             \
        new NanoMipsInsProperty(name, extractTReg, convertTReg, isValidTReg,   \
                                extractSReg, convertSReg, isValidSReg);        \
    insMap[opcode] = insProp;                                                  \
    assert(insMap.count(opcode));                                              \
  }

#include "NanoMipsRelocInsProperty.inc"

#undef INS_PROPERTY
#undef EXTRACT_REG

#undef INSERT_REG
#undef INS_TEMPLATE
#undef TRANSFORM_TEMPLATE
#define INSERT_REG(Pos, Size, Reg) insertReg<Pos, Size, RM_NANOMIPS_##Reg>
#define INS_TEMPLATE(name, opcode, reloc, insSize, insertTReg, insertSReg)     \
  NanoMipsInsTemplate(name, opcode, R_NANOMIPS_##reloc, insSize, insertTReg,   \
                      insertSReg)
#define TRANSFORM_TEMPLATE(type, opcode, relocs, insTemplates)                 \
  {                                                                            \
    static const NanoMipsInsTemplate insTemplateArray[] = insTemplates;        \
    static const RelType relocArray[] = relocs;                                \
    uint32_t insCount =                                                        \
        sizeof(insTemplateArray) / sizeof(NanoMipsInsTemplate);                \
    uint32_t relocCount = sizeof(relocArray) / sizeof(RelType);                \
    NanoMipsTransformTemplate *transformTemplate =                             \
        new NanoMipsTransformTemplate(type, insTemplateArray, insCount,        \
                                      relocArray, relocCount);                 \
    assert(insMap.count(opcode));                                              \
    insMap[opcode]->addTransform(transformTemplate, type, relocArray,          \
                                 relocCount);                                  \
  }

#include "NanoMipsRelocInsProperty.inc"

#undef TRANSOFRM_TEMPLATE
#undef INS_TEMPLATE
#undef INSERT_REG
}

std::string NanoMipsInsPropertyTable::toString() const {
  std::string tmp;
  raw_string_ostream SS(tmp);
  for (auto it = insMap.begin(); it != insMap.end(); it++) {
    uint32_t opcode = it->first;
    const NanoMipsInsProperty *insProp = it->second;
    SS << "Opcode: " << utohexstr(opcode) << ":\n\t" << insProp->toString()
       << "\n";
  }
  return SS.str();
}

NanoMipsInsPropertyTable::~NanoMipsInsPropertyTable() {
  // C++ 14
  llvm::for_each(insMap, [](auto &pair) {
    delete pair.second;
    pair.second = nullptr;
  });
  // for(auto it = insMap.begin(); it != insMap.end(); it++)
  // {
  //   delete it->second;
  //   it->second = nullptr;
  // }
}

NanoMipsInsProperty *
NanoMipsInsPropertyTable::findInsProperty(uint64_t insn, uint64_t mask,
                                          RelType reloc) const {
  NanoMipsInsProperty *insProperty = this->insMap.lookup(insn & mask);
  if (insProperty && insProperty->hasReloc(reloc))
    return insProperty;
  else
    return nullptr;
}

// NanoMipsTransform

std::string NanoMipsTransform::getTypeAsString() const {
  switch (this->getType()) {
  case NanoMipsTransform::TransformNone:
    return "TransformNone";
  case NanoMipsTransform::TransformRelax:
    return "TransformRelax";
  case NanoMipsTransform::TransformExpand:
    return "TransformExpand";
    // Commented this not to generate a warning
    // default:
    //   llvm_unreachable("Nota a valid transform type (nanoMIPS)");
  }
}

void NanoMipsTransform::changeBytes(InputSection *isec, uint64_t location,
                                    int32_t count) {
  assert(location <= isec->size &&
         "Location mustn't be larger than size of section");
  assert((count > 0 || location >= static_cast<uint64_t>(-count)) &&
         "Number of deleted bytes must be less than location");

  if ((count > 0 &&
       static_cast<uint32_t>(count) > isec->nanoMipsRelaxAux->freeBytes) ||
      !isec->nanoMipsRelaxAux->isAlreadyTransformed) {
    isec->nanoMipsRelaxAux->isAlreadyTransformed = true;
    size_t newSize = isec->size + count;
    uint8_t *newData;
    // TODO: Check if the mutex is necessary as it is used only for nanoMIPS
    // relaxations which shouldn't be concurrent
    {
      static std::mutex mu;
      std::lock_guard<std::mutex> lock(mu);
      // TODO: Check if this can eat up a lot of memory
      // as bptr allocator only allocates, it doesn't free
      // the memory
      // Allocate more data just in case
      // TODO: Probably shouldn't allocate twice as much, but leave it as is for
      // now
      newData = context().bAlloc.Allocate<uint8_t>(newSize * 2);
    }
    if (newData == nullptr)
      fatal(toString(isec) + " allocation of new size failed");

    memcpy(newData, isec->content_, location);
    memcpy(newData + location + count, isec->content_ + location,
           isec->size - location);
    isec->nanoMipsRelaxAux->freeBytes = newSize;
    isec->content_ = newData;
    isec->size = newSize;
  } else {
    // TODO: See if this can be done backwards as well, maybe that is faster
    // (use memcpy) Possible implementation uint8_t *beginSrc =
    // const_cast<uint8_t *>(this->content_) + location; uint8_t *endSrc =
    // beginSrc + this->size - bytesDropped - location; uint8_t *endDst = endSrc
    // + count; std::reverse_copy<uint8_t *, uint8_t *>(beginSrc, endSrc,
    // endDst);
    memmove(const_cast<uint8_t *>(isec->content_) + location + count,
            isec->content_ + location, isec->size - location);
    isec->nanoMipsRelaxAux->freeBytes -= count;
    isec->size += count;
  }
}

void NanoMipsTransform::updateSectionContent(InputSection *isec,
                                             uint64_t location, int32_t delta,
                                             bool align) {

  changeBytes(isec, location, delta);

  this->changed = true;
  this->changedThisIteration = true;
  LLVM_DEBUG(llvm::dbgs() << "Changed size of input section "
                          << (isec->file ? isec->file->getName() : "None")
                          << ":" << isec->name << " by " << delta
                          << " on location 0x" << utohexstr(location)
                          << (align ? " due to alignment\n" : "\n"););

  // TODO: This is not efficient maybe sort relocs by offset, and just traverse
  // over the ones which have offset larger or equal than the processed one
  for (auto &reloc : isec->relocs()) {
    // TODO: Need to make this different for align, fill and max reloc
    // but that will be done after they are implemented
    // Need to skip these 3 relocs if the change is due to alignment,
    // which it is if align == true
    if (align && reloc.offset == location &&
        (reloc.type == R_NANOMIPS_ALIGN || reloc.type == R_NANOMIPS_FILL ||
         reloc.type == R_NANOMIPS_MAX))
      continue;

    if (reloc.offset >= location)
      reloc.offset += delta;
  }

  if (isec->file) {
    for (auto &symAnchor : isec->nanoMipsRelaxAux->anchors) {
      auto *dSym = symAnchor.d;
      auto *symSec = dyn_cast_or_null<InputSection>(dSym->section);
      if (symSec && symSec == isec) {
        if (!symAnchor.end && dSym->value >= location) {
          LLVM_DEBUG(llvm::dbgs() << "Changed symbol value: " << dSym->getName()
                                  << " by " << delta << "\n";);
          dSym->value += delta;
        } else if (symAnchor.end && dSym->value < location &&
                   dSym->value + dSym->size >= location) {
          LLVM_DEBUG(llvm::dbgs() << "Changed symbol size: " << dSym->getName()
                                  << " by " << delta << "\n";);
          dSym->size += delta;
        }
      }
    }
  }
}

void NanoMipsTransform::transform(
    Relocation *reloc, const NanoMipsTransformTemplate *transformTemplate,
    const NanoMipsInsProperty *insProperty,
    const NanoMipsRelocProperty *relocProperty, InputSection *isec,
    uint64_t insn, uint32_t &relNum) const {
  uint32_t tReg = 0;
  uint32_t sReg = 0;
  switch (reloc->type) {
  case R_NANOMIPS_PC14_S1: {
    tReg = insProperty->getTReg(insn);
    sReg = insProperty->getSReg(insn);
    if (transformTemplate->getType() == TT_NANOMIPS_PCREL16) {
      uint32_t tReg16 = tReg & 0x7;
      uint32_t sReg16 = sReg & 0x7;
      if ((insProperty->getName() == "beqc" && sReg16 > tReg16) ||
          (insProperty->getName() == "bnec" && sReg16 < tReg16)) {
        uint32_t tmp = tReg;
        tReg = sReg;
        sReg = tmp;
      }
    }
    break;
  }
  case R_NANOMIPS_PC21_S1: {
    tReg = insProperty->getTReg(insn);
    if (insProperty->getName() == "move.balc")
      sReg = insProperty->getSReg(insn);
    else
      sReg = tReg;
    break;
  }

  case R_NANOMIPS_PC4_S1:
  case R_NANOMIPS_LO4_S2: {
    tReg = insProperty->convTReg(insn);
    sReg = insProperty->convSReg(insn);
    break;
  }
  case R_NANOMIPS_PC_I32:
  case R_NANOMIPS_I32: {
    tReg = insProperty->getTReg(insn);
    // Needed for addu 32 to fill spots of two registers rt and rs
    // rd is sReg here
    tReg = tReg << 5 | tReg;
    sReg = config->nanoMipsExpandReg;
    break;
  }

  case R_NANOMIPS_GPREL19_S2:
  case R_NANOMIPS_GPREL18:
  case R_NANOMIPS_GPREL17_S1: {
    tReg = insProperty->getTReg(insn);
    sReg = config->nanoMipsExpandReg;
    break;
  }

  case R_NANOMIPS_PC25_S1: {
    tReg = config->nanoMipsExpandReg;
    sReg = config->nanoMipsExpandReg;
    break;
  }
  case R_NANOMIPS_PC10_S1: {
    tReg = 0;
    sReg = 0;
    break;
  }
  case R_NANOMIPS_PC7_S1:
  case R_NANOMIPS_GPREL7_S2: {
    tReg = insProperty->convTReg(insn);
    sReg = 0;
    break;
  }
  case R_NANOMIPS_PC11_S1:
  case R_NANOMIPS_LO12: {
    tReg = insProperty->getTReg(insn);
    // sReg is imm or bit value here
    sReg = insProperty->getSReg(insn);
    break;
  }
  case R_NANOMIPS_GPREL_I32: {
    tReg = insProperty->getTReg(insn);
    sReg = 0;
    break;
  }
  default:
    // Should be unreachable, but for now just break and return
    LLVM_DEBUG(llvm::dbgs() << "Transform for relocation: " << reloc->type
                            << " not supported yet!\n");
    return;
  }

  uint32_t offset = reloc->offset - (relocProperty->getInstSize() == 6 ? 2 : 0);

  // Whether we are inserting a new reloc, or just changing the existing one
  bool newReloc = false;
  RelType oldRelType = reloc->type;
  auto instructionList =
      ArrayRef(transformTemplate->getInsns(), transformTemplate->getInsCount());
  for (auto &insTemplate : instructionList) {
    uint64_t newInsn = insTemplate.getInstruction(tReg, sReg);
    RelType newRelType = insTemplate.getReloc();

    if (newRelType != R_NANOMIPS_NONE) {
      assert(transformTemplate->getType() != TT_NANOMIPS_DISCARD &&
             "There is a reloc for a DISCARD relaxation!");

      uint32_t newROffset = (insTemplate.getSize() == 6 ? offset + 2 : offset);
      if (!newReloc) {
        reloc->offset = newROffset;
        reloc->type = newRelType;
        // Only param needed is relType, other ones are not important for
        // nanoMIPS
        reloc->expr = target->getRelExpr(newRelType, *reloc->sym,
                                         isec->content().data() + newROffset);
        newReloc = true;
        LLVM_DEBUG(llvm::dbgs()
                       << "Changed current reloc to " << reloc->type << "\n";);
      } else {
        Relocation newRelocation;
        newRelocation.addend = reloc->addend;
        newRelocation.offset = newROffset;
        // Only param needed is relType, other ones are not important for
        // nanoMIPS
        newRelocation.expr = target->getRelExpr(
            newRelType, *reloc->sym, isec->content().data() + newROffset);
        newRelocation.sym = reloc->sym;
        newRelocation.type = newRelType;
        relNum++;
        // TODO: See how you will insert relocations later, as relocations won't
        // be SmallVector anymore
        isec->relocations.insert(isec->relocations.begin() + relNum,
                                 newRelocation);
        // Because we add a relocation, it might invalidate our previous reloc!
        reloc = &isec->relocations[relNum - 1];
        LLVM_DEBUG(llvm::dbgs() << "Added new reloc " << newRelType << "\n";);
      }
    }

    newInsns.emplace_back(newInsn, offset, insTemplate.getSize());
    LLVM_DEBUG(llvm::dbgs() << "New instruction " << insTemplate.getName()
                            << ": 0x" << utohexstr(newInsn) << " to offset: 0x"
                            << utohexstr(offset) << "\n");
    offset += insTemplate.getSize();
  }

  // We are adding a symbol for branch over the bc instruction, as this
  // generates a negative branch + bc32 and the negative branch needs to skip
  // bc32
  if ((oldRelType == R_NANOMIPS_PC14_S1 || oldRelType == R_NANOMIPS_PC11_S1) &&
      transformTemplate->getType() == TT_NANOMIPS_PCREL32_LONG) {
    newSymCount++;
    StringRef nameRef = saver().save(Twine("__skip_bc__") + Twine(newSymCount));
    Defined *s = dyn_cast<Defined>(
        symtab.addSymbol(Defined{isec->file, nameRef, STB_GLOBAL, STV_HIDDEN,
                                 STT_NOTYPE, offset, 0, isec}));
    if (!s) {
      error("Can't create needed symbol for relaxations/expansions!");
      exitLld(1);
    }
    reloc->sym = s;
    isec->nanoMipsRelaxAux->anchors.push_back({s, false});
    in.symTab->addSymbol(s);
    LLVM_DEBUG(llvm::dbgs()
                   << "New symbol " << s->getName() << " in section "
                   << (isec->file ? isec->file->getName() : "no file") << ":"
                   << isec->name << " on offset " << offset << "\n";);
  }
}

// NanoMipsTransformRelax

const NanoMipsInsProperty *
NanoMipsTransformRelax::getInsProperty(uint64_t insn, uint64_t insnMask,
                                       RelType reloc,
                                       InputSectionBase *isec) const {
  // Can't relax 32-bit to 16-bit instructions if
  // --insn32 option is passed
  if (config->nanoMipsInsn32) {
    switch (reloc) {
    case R_NANOMIPS_PC_I32:
    case R_NANOMIPS_GPREL_I32:
      break;
    default:
      return nullptr;
    }
  }

  const NanoMipsInsProperty *insProperty =
      this->insPropertyTable->findInsProperty(insn, insnMask, reloc);
  if (!insProperty)
    return nullptr;

  switch (reloc) {
  case R_NANOMIPS_PC14_S1: {
    uint32_t sReg = insProperty->getSReg(insn);
    if (!insProperty->hasTransform(TT_NANOMIPS_PCREL16, reloc) ||
        !isec->nanoMipsRelaxAux->fullNanoMipsISA ||
        (!insProperty->areRegsValid(insn) &&
         !(insProperty->hasTransform(TT_NANOMIPS_PCREL16_ZERO, reloc) &&
           insProperty->isTRegValid(insn) && sReg == 0)))
      return nullptr;

    unsigned int tReg = insProperty->getTReg(insn);
    StringRef insName = insProperty->getName();
    // beqc cannot be relaxed to beqc[16] if
    // sReg and tReg are same
    // TODO: Maybe the same needs to be done for bnec
    if ((insName == "beqc" || insName == "bnec") && tReg == sReg)
      return nullptr;
    return insProperty;
  }
  case R_NANOMIPS_PC_I32: {
    if (insProperty->hasTransform(TT_NANOMIPS_PCREL32, reloc))
      return insProperty;
    return nullptr;
  }
  case R_NANOMIPS_PC25_S1: {
    if (insProperty->hasTransform(TT_NANOMIPS_PCREL16, reloc))
      return insProperty;
    return nullptr;
  }
  case R_NANOMIPS_LO12: {
    if (config->nanoMipsRelaxLo12 &&
        insProperty->hasTransform(TT_NANOMIPS_ABS16, reloc) &&
        insProperty->areRegsValid(insn))
      return insProperty;

    return nullptr;
  }
  case R_NANOMIPS_GPREL19_S2: {
    if (insProperty->hasTransform(TT_NANOMIPS_GPREL16, reloc) &&
        insProperty->isTRegValid(insn))
      return insProperty;

    return nullptr;
  }
  case R_NANOMIPS_GPREL_I32: {
    if (insProperty->hasTransform(TT_NANOMIPS_GPREL32, reloc) ||
        insProperty->hasTransform(TT_NANOMIPS_GPREL32_WORD, reloc))
      return insProperty;

    return nullptr;
  }
  default:
    // TODO: Should be just break, but after all relocs are processed
    // I will change this
    return nullptr;
    break;
  }

  return insProperty;
}

const NanoMipsTransformTemplate *NanoMipsTransformRelax::getTransformTemplate(
    const NanoMipsInsProperty *insProperty, const Relocation &reloc,
    uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const {
  const uint32_t bits = config->wordsize * 8;
  uint64_t gpVal = SignExtend64(ElfSym::mipsGp->value, bits);
  // TODO: Maybe it is not possible to have val & 0x1 not equal to 0 at some
  // cases check this, so this if case can be relaxed
  switch (reloc.type) {
  case R_NANOMIPS_PC14_S1: {
    // TODO: Need to check if shortening instruction messed up the range.
    // End of instruction is used for pcrel jumps.
    uint64_t val = valueToRelocate - 4;
    uint32_t sReg = insProperty->getSReg(insn);
    if (val != 0 && sReg != 0 && isUInt<5>(val) && ((val & 0x1) == 0))
      return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL16, reloc.type);
    else if (sReg == 0 && isInt<8>(val) && ((val & 0x1) == 0))
      return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL16_ZERO,
                                               reloc.type);
    else
      return nullptr;
  }
  case R_NANOMIPS_PC_I32: {
    uint64_t val = valueToRelocate - 4;
    // Adjust value if this is a backward branch
    if (static_cast<int64_t>(val) < 0)
      val += 2;
    if ((val & 0x1) == 0 && isInt<22>(val))
      return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL32, reloc.type);
    else
      return nullptr;
  }
  case R_NANOMIPS_PC25_S1: {
    uint64_t val = valueToRelocate - 4;
    // Adjust value if this is a backward branch
    if (static_cast<int64_t>(val) < 0)
      val += 2;

    if (((val & 0x1) == 0) && isInt<11>(val))
      return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL16, reloc.type);
    else
      return nullptr;
  }
  case R_NANOMIPS_LO12: {
    uint64_t val = valueToRelocate & 0xfff;
    if (((val & 0x3) == 0) && isUInt<6>(val))
      return insProperty->getTransformTemplate(TT_NANOMIPS_ABS16, reloc.type);
    else
      return nullptr;
  }
  case R_NANOMIPS_GPREL19_S2: {
    uint64_t val = valueToRelocate;
    if ((gpVal != -1ULL) && (((val & 0x3) == 0) && isUInt<9>(val)))
      return insProperty->getTransformTemplate(TT_NANOMIPS_GPREL16, reloc.type);
    else
      return nullptr;
  }
  case R_NANOMIPS_GPREL_I32: {
    uint64_t val = valueToRelocate;

    if (gpVal == -1ULL)
      return nullptr;

    else if (((val & 0x3) != 0) && isUInt<18>(val))
      return insProperty->getTransformTemplate(TT_NANOMIPS_GPREL32, reloc.type);

    else if (((val & 0x3) == 0) && isUInt<21>(val))
      return insProperty->getTransformTemplate(TT_NANOMIPS_GPREL32_WORD,
                                               reloc.type);

    else
      return nullptr;
  }
  default:
    // TODO: Should be unreachable when all relocs are processed
    break;
  }
  return nullptr;
}

// NanoMipsTransformExpand

const NanoMipsInsProperty *lld::elf::NanoMipsTransformExpand::getInsProperty(
    uint64_t insn, uint64_t insnMask, RelType reloc,
    InputSectionBase *isec) const {
  // TODO: Relax checks for nanoMips insn32, should that be done here? Check it.
  switch (reloc) {
  case R_NANOMIPS_PC21_S1:
  case R_NANOMIPS_PC14_S1:
  case R_NANOMIPS_PC4_S1:
  case R_NANOMIPS_PC_I32:
  case R_NANOMIPS_GPREL19_S2:
  case R_NANOMIPS_GPREL18:
  case R_NANOMIPS_GPREL17_S1:
  case R_NANOMIPS_PC10_S1:
  case R_NANOMIPS_PC7_S1:
  case R_NANOMIPS_PC25_S1:
  case R_NANOMIPS_PC11_S1:
  case R_NANOMIPS_LO4_S2:
  case R_NANOMIPS_I32:
  case R_NANOMIPS_GPREL7_S2:
    return insPropertyTable->findInsProperty(insn, insnMask, reloc);
  default:
    break;
  }

  return nullptr;
}

const NanoMipsTransformTemplate *
lld::elf::NanoMipsTransformExpand::getTransformTemplate(
    const NanoMipsInsProperty *insProperty, const Relocation &reloc,
    uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const {
  // TODO: Maybe it is not possible to have val & 0x1 not equal to 0 at some
  // cases check this
  // TODO: Check if pcrel and/or abs is respected well
  const uint32_t bits = config->wordsize * 8;
  uint64_t gpVal = SignExtend64(ElfSym::mipsGp->value, bits);
  switch (reloc.type) {
  case R_NANOMIPS_PC21_S1: {
    uint64_t val = valueToRelocate - 4;
    if (((val & 0x1) == 0) && isInt<22>(val))
      return nullptr;
    break;
  }
  case R_NANOMIPS_PC14_S1: {
    uint64_t val = valueToRelocate - 4;
    if (((val & 0x1) == 0) && isInt<15>(val))
      return nullptr;
    break;
  }
  case R_NANOMIPS_PC4_S1: {
    uint64_t val = valueToRelocate - 2;
    if (((val & 0x1) == 0) && isUInt<5>(val))
      return nullptr;
    break;
  }
  case R_NANOMIPS_PC_I32: {
    uint64_t val = valueToRelocate - 4;
    if (!valueNeedsHw110880Fix(val))
      return nullptr;
    break;
  }
  case R_NANOMIPS_I32: {
    uint64_t val = valueToRelocate;
    if (!valueNeedsHw110880Fix(val))
      return nullptr;
    break;
  }
  case R_NANOMIPS_GPREL19_S2: {
    uint64_t val = valueToRelocate;
    if (gpVal == -1ULL || (isUInt<21>(val) && ((val & 0x3) == 0)))
      return nullptr;
    break;
  }
  case R_NANOMIPS_GPREL18: {
    uint64_t val = valueToRelocate;
    if (gpVal == -1ULL || isUInt<18>(val))
      return nullptr;
    else if (gpVal != -1ULL && isUInt<21>(val) && ((val & 0x3) == 0) &&
             insProperty->hasTransform(TT_NANOMIPS_GPREL32_WORD, reloc.type))
      return insProperty->getTransformTemplate(TT_NANOMIPS_GPREL32_WORD,
                                               reloc.type);
    break;
  }
  case R_NANOMIPS_GPREL17_S1: {
    uint64_t val = valueToRelocate;
    if (gpVal == -1ULL || (isUInt<18>(val) && ((val & 0x1) == 0)))
      return nullptr;
    break;
  }
  case R_NANOMIPS_PC10_S1: {
    uint64_t val = valueToRelocate - 2;
    if (isInt<11>(val) && ((val & 0x1) == 0))
      return nullptr;
    break;
  }
  case R_NANOMIPS_PC7_S1: {
    uint64_t val = valueToRelocate - 2;
    if (isInt<8>(val) && ((val & 0x1) == 0))
      return nullptr;
    break;
  }
  case R_NANOMIPS_PC25_S1: {
    uint64_t val = valueToRelocate - 4;
    if (isInt<26>(val) && ((val & 0x1) == 0)) {
      // Copied from gold
      // The offset in backward branch should be less than or equal to 0xffff.
      // The offset in forward branch should be greater than or equal to
      // 0xff0000. The range is (-33423362, 33423360).
      if (config->nanoMipsFixHw113064) {
        int64_t signedVal = SignExtend64(val, bits);
        if (signedVal <= SignExtend64(0xfe01fffe, bits) ||
            signedVal >= SignExtend64(0x1fe0000, bits))
          // Expand to lapc+jalrc
          return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL_NMF,
                                                   reloc.type);
      }
      return nullptr;
    }

    break;
  }
  case R_NANOMIPS_PC11_S1: {
    uint64_t val = valueToRelocate - 4;
    if (isInt<12>(val) && ((val & 0x1) == 0))
      return nullptr;

    break;
  }
  case R_NANOMIPS_LO4_S2: {
    uint64_t val = valueToRelocate & 0xfff;
    if (isUInt<6>(val) && ((val & 0x3) == 0))
      return nullptr;

    break;
  }
  case R_NANOMIPS_GPREL7_S2: {
    uint64_t val = valueToRelocate;
    if (gpVal == -1ULL || (isUInt<9>(val) && ((val & 0x3) == 0)))
      return nullptr;
    break;
  }

  default:
    // TODO: Should be unreachable when all relocs are processed
    LLVM_DEBUG(llvm::dbgs() << "Relocation: " << reloc.type
                            << " not supported yet for expansions\n";);
    return nullptr;
  }
  return getExpandTransformTemplate(insProperty, reloc, insn, isec);
}

const NanoMipsTransformTemplate *
lld::elf::NanoMipsTransformExpand::getExpandTransformTemplate(
    const NanoMipsInsProperty *insProperty, const Relocation &reloc,
    uint64_t insn, const InputSection *isec) const {
  // TODO: Test all versions
  bool nanoMipsFullAbi = isec->nanoMipsRelaxAux->fullNanoMipsISA;
  bool pcrel = isec->nanoMipsRelaxAux->pcrel;
  switch (reloc.type) {
  case R_NANOMIPS_PC21_S1: {
    if (insProperty->getName() == "move.balc")
      // TODO: See how this goes with insn32 option, as this transformation
      // generates 16bit move with balc
      return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL32_LONG,
                                               reloc.type);
    else if (nanoMipsFullAbi)
      return pcrel ? insProperty->getTransformTemplate(TT_NANOMIPS_PCREL_NMF,
                                                       reloc.type)
                   : insProperty->getTransformTemplate(TT_NANOMIPS_ABS_NMF,
                                                       reloc.type);
    else
      return pcrel ? insProperty->getTransformTemplate(TT_NANOMIPS_PCREL32_LONG,
                                                       reloc.type)
                   : insProperty->getTransformTemplate(TT_NANOMIPS_ABS32_LONG,
                                                       reloc.type);
  }
  case R_NANOMIPS_PC14_S1:
  case R_NANOMIPS_PC11_S1:
    // Opposite branch transformation
    return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL32_LONG,
                                             reloc.type);

  case R_NANOMIPS_PC4_S1:
    // Equality should be impossible
    return insProperty->getSReg(insn) > insProperty->getTReg(insn)
               ? insProperty->getTransformTemplate(TT_NANOMIPS_BNEC32,
                                                   reloc.type)
               : insProperty->getTransformTemplate(TT_NANOMIPS_BEQC32,
                                                   reloc.type);

  case R_NANOMIPS_PC_I32:
  case R_NANOMIPS_I32:
    return insProperty->getTransformTemplate(TT_NANOMIPS_IMM48_FIX, reloc.type);
  case R_NANOMIPS_GPREL19_S2:
  case R_NANOMIPS_GPREL18:
  case R_NANOMIPS_GPREL17_S1: {
    if (insProperty->getName().contains("addiu")) {
      // Transforms for addiu[gp.w] and addiu[gp.b]
      if (nanoMipsFullAbi)
        return insProperty->getTransformTemplate(TT_NANOMIPS_GPREL_NMF,
                                                 reloc.type);
      else if (config->nanoMipsStrictAddressModes)
        return insProperty->getTransformTemplate(TT_NANOMIPS_GPREL_LONG,
                                                 reloc.type);
      else
        return pcrel ? insProperty->getTransformTemplate(
                           TT_NANOMIPS_PCREL32_LONG, reloc.type)
                     : insProperty->getTransformTemplate(TT_NANOMIPS_ABS32_LONG,
                                                         reloc.type);
    } else {
      if (config->nanoMipsStrictAddressModes)
        return nanoMipsFullAbi
                   ? insProperty->getTransformTemplate(TT_NANOMIPS_GPREL32_NMF,
                                                       reloc.type)
                   : insProperty->getTransformTemplate(TT_NANOMIPS_GPREL_LONG,
                                                       reloc.type);
      else if (nanoMipsFullAbi &&
               insProperty->hasTransform(TT_NANOMIPS_PCREL_NMF, reloc.type))
        return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL_NMF,
                                                 reloc.type);
      else
        return pcrel ? insProperty->getTransformTemplate(
                           TT_NANOMIPS_PCREL32_LONG, reloc.type)
                     : insProperty->getTransformTemplate(TT_NANOMIPS_ABS32_LONG,
                                                         reloc.type);
    }
  }
  case R_NANOMIPS_PC10_S1:
  case R_NANOMIPS_PC7_S1:
    return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL32, reloc.type);

  case R_NANOMIPS_PC25_S1: {
    // TODO: Check if logic for insn32 is right
    if (nanoMipsFullAbi)
      return pcrel ? insProperty->getTransformTemplate(TT_NANOMIPS_PCREL_NMF,
                                                       reloc.type)
                   : insProperty->getTransformTemplate(TT_NANOMIPS_ABS_NMF,
                                                       reloc.type);

    else if (!pcrel)
      return config->nanoMipsInsn32
                 ? insProperty->getTransformTemplate(TT_NANOMIPS_ABS32_LONG,
                                                     reloc.type)
                 : insProperty->getTransformTemplate(TT_NANOMIPS_ABS16_LONG,
                                                     reloc.type);

    else
      return config->nanoMipsInsn32
                 ? insProperty->getTransformTemplate(TT_NANOMIPS_PCREL32_LONG,
                                                     reloc.type)
                 : insProperty->getTransformTemplate(TT_NANOMIPS_PCREL16_LONG,
                                                     reloc.type);
  }
  case R_NANOMIPS_LO4_S2:
    return insProperty->getTransformTemplate(TT_NANOMIPS_ABS32, reloc.type);

  case R_NANOMIPS_GPREL7_S2:
    return insProperty->getTransformTemplate(TT_NANOMIPS_GPREL32_WORD,
                                             reloc.type);

  default:
    // Should be unreachable when all relocs are processed
    LLVM_DEBUG(llvm::dbgs() << "Relocation: " << reloc.type
                            << " not supported yet for expansions\n";);
    return nullptr;
  }
}

// NanoMipsTransformController

void NanoMipsTransformController::initState() {
  if (config->relax) {
    this->currentState = &this->transformRelax;
  } else if (config->expand)
    this->currentState = &this->transformExpand;
  else
    this->currentState = &this->transformNone;

  LLVM_DEBUG(llvm::dbgs() << "Initial state of transform: "
                          << this->currentState->getTypeAsString() << "\n");

  return;
}

void NanoMipsTransformController::changeState(int pass) {

  if (this->currentState->getChangedThisIteration()) {
    // We want to repeat the transformation until it doesn't change anything in
    // iterations
    this->currentState->resetChangedThisIteration();
    return;
  }
  if ((notExpandedYet || this->currentState->getChanged()) &&
      this->currentState->getType() == NanoMipsTransform::TransformRelax &&
      config->expand) {
    notExpandedYet = false;
    this->currentState->resetChanged();
    this->currentState = &this->transformExpand;
    LLVM_DEBUG(llvm::dbgs() << "Changed transform state to Expand\n");
    return;
  }

  if (this->currentState->getChanged() &&
      this->currentState->getType() == NanoMipsTransform::TransformExpand &&
      config->relax && pass < relaxPassLimit) {
    this->currentState->resetChanged();
    this->currentState = &this->transformRelax;
    LLVM_DEBUG(llvm::dbgs() << "Changed transform state to Relax\n");
    return;
  }

  this->currentState->resetChanged();
  this->currentState = &this->transformNone;
  LLVM_DEBUG(llvm::dbgs() << "Changed transform state to None\n";);
}