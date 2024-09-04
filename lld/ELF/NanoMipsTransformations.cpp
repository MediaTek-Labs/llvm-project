//===- NanoMipsTransformations.cpp
//-----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "NanoMipsTransformations.h"
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

#include "Target.h"

#define DEBUG_TYPE "lld-nanomips"
using namespace lld;
using namespace lld::elf;
using namespace llvm;
using namespace llvm::ELF;
using namespace llvm::object;
using namespace llvm::support::endian;

static constexpr const uint32_t bc32Size = 4;

// \2 is used to ensure that the symbol is unique
static constexpr const char *skipBcSymPrefix = "__skip_bc_\2_";

// Function templates for insert, extract, convert and isValid Reg

enum NanoMipsRegisterMapping { RM_NANOMIPS_TREG, RM_NANOMIPS_SREG };

template <uint32_t Pos, uint32_t Size, NanoMipsRegisterMapping REG>
inline uint64_t insertReg(uint32_t treg, uint32_t sreg, uint64_t data) {
  uint32_t reg;

  if (REG == RM_NANOMIPS_TREG)
    reg = treg;
  else
    reg = sreg;

  return data | ((reg & ((1U << Size) - 1)) << Pos);
}

template <uint32_t Pos, uint32_t Size>
inline uint32_t extractReg(uint64_t data) {
  return (data >> Pos) & ((1U << Size) - 1);
}

uint32_t moveBalcTReg(uint64_t data) {
  uint32_t reg = ((data >> 21) & 0x7) | ((data >> 22) & 0x8);
  static uint32_t gpr4ZeroMap[] = {8,  9,  10, 0,  4,  5,  6,  7,
                                   16, 17, 18, 19, 20, 21, 22, 23};
  return gpr4ZeroMap[reg];
}

// We'll call it SReg even though it is a DReg because every ins has a TReg and
// an SReg in ins properties
inline uint32_t moveBalcSReg(uint64_t data) { return ((data >> 24) & 0x1) + 4; }

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
inline bool isRegValid(uint32_t reg) {
  return ((4 <= reg && reg <= 7) || (16 <= reg && reg <= 19));
}

// Check if a 5-bit store source register can be converted to a 3-bit one
inline bool isStoreSrcRegValid(uint32_t reg) {
  return (reg == 0 || (4 <= reg && reg <= 7) || (17 <= reg && reg <= 19));
}

// Functions for checking if some instructions need to be fixed

static inline bool valueNeedsHw110880Fix(uint64_t val) {
  if (!config->nanoMipsFixHw110880)
    return false;
  return ((val & 0x50016400) == 0x50012400) &&
         (((val >> 24) & 0x7) == 0x1 || ((val >> 24) & 0x2) == 0x2);
}

// Return rt and u[11:3] fields from nanoMIPS save/restore instruction

inline uint32_t extractSaveResFields(uint64_t data) {
  return (((data >> 3) & 0x1ff) | ((data >> 12) & 0x3e00));
}

// Insert rt and u[11:3] fields in nanoMIPS save/restore instruction
// sreg contains fields
inline uint64_t insertSaveResFields(uint32_t treg, uint32_t sreg,
                                    uint64_t data) {
  uint32_t u = ((sreg & 0x1ff) << 3);
  uint32_t rt = ((sreg & 0x3e00) << 12);
  return (data | rt | u);
}

// Insert rt and u[7:4] fields in nanoMIPS save[16]/restore.jrc[16] instruction
// sreg contains fields
inline uint64_t insertSaveRes16Fields(uint32_t treg, uint32_t sreg,
                                      uint64_t data) {
  uint32_t u = ((sreg & 0x1ff) << 3);
  uint32_t rt = ((sreg & 0x3e00) >> 9);
  uint32_t rt1 = (rt == 30) ? 0 : (1U << 9);
  return (data | rt1 | u);
}

// Function for adding new defined synthetic linker symbol
Defined *addSyntheticLinkerSymbol(const Twine &name, uint64_t val,
                                  uint64_t size, SectionBase *sec) {
  StringRef symName = saver().save(name);
  Defined *s = dyn_cast<Defined>(symtab.addSymbol(Defined{
      nullptr, symName, STB_GLOBAL, STV_DEFAULT, STT_NOTYPE, val, size, sec}));

  if (!s) {
    error("Can't create needed symbol for relaxations/expansions!");
    exitLld(1);
  }

  in.symTab->addSymbol(s);
  return s;
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
#include "NanoMipsTransformationProperties.inc"
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
  SS << name << ", " << utohexstr(data) << ", " << reloc << ", " << size;
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

  for (uint32_t i = 0; i < relocCount; i++)
    this->relocs.insert(relocs[i]);

  transformationMap[type] = transformTemplate;
}

std::string NanoMipsInsProperty::toString() const {
  std::string tmp;
  raw_string_ostream SS(tmp);
  SS << name;
  return SS.str();
}

NanoMipsInsProperty::~NanoMipsInsProperty() {
  llvm::for_each(transformationMap, [](auto &pair) {
    delete pair.second;
    pair.second = nullptr;
  });
}

// NanoMipsInsPropertyTable

NanoMipsInsPropertyTable::NanoMipsInsPropertyTable() {
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

#include "NanoMipsTransformationProperties.inc"

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

#include "NanoMipsTransformationProperties.inc"

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
  llvm::for_each(insMap, [](auto &pair) {
    delete pair.second;
    pair.second = nullptr;
  });
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

uint32_t NanoMipsTransform::newSkipBcSymCount = 0;

std::string NanoMipsTransform::getTypeAsString() const {
  switch (this->getType()) {
  case NanoMipsTransform::TransformNone:
    return "TransformNone";
  case NanoMipsTransform::TransformRelax:
    return "TransformRelax";
  case NanoMipsTransform::TransformExpand:
    return "TransformExpand";
  default:
    llvm_unreachable("Invalid transform state");
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

    // Note: The size of newSize could and should be reduced
    uint8_t *newData = context().bAlloc.Allocate<uint8_t>(newSize * 2);
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
    // for relaxations (std::reverse_copy maybe)
    memmove(const_cast<uint8_t *>(isec->content_) + location + count,
            isec->content_ + location, isec->size - location);
    isec->nanoMipsRelaxAux->freeBytes -= count;
    isec->size += count;
  }
}

void NanoMipsTransform::updateSectionContent(InputSection *isec,
                                             uint64_t location, int32_t delta,
                                             bool align) {

  // Other than increasing/decreasing byte size of isec, it also
  // allocates new section content if delta is 0 and section
  // hasn't yet been changed, as those content would be readonly
  changeBytes(isec, location, delta);
  if (delta == 0)
    return;

  this->changed = true;
  this->changedThisIteration = true;
  LLVM_DEBUG(llvm::dbgs() << "Changed size of input section "
                          << (isec->file ? isec->file->getName() : "None")
                          << ":" << isec->name << " by " << delta
                          << " on location 0x" << utohexstr(location)
                          << (align ? " due to alignment\n" : "\n"););

  for (auto &reloc : isec->relocs()) {
    // Need to skip these 3 relocs if the change is due to alignment
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

SmallVector<NewInsnToWrite, 3> NanoMipsTransform::getTransformInsns(
    Relocation *reloc, const NanoMipsTransformTemplate *transformTemplate,
    const NanoMipsInsProperty *insProperty,
    const NanoMipsRelocProperty *relocProperty, InputSection *isec,
    uint64_t insn, uint32_t relNum) const {
  uint32_t tReg = 0;
  uint32_t sReg = 0;
  SmallVector<NewInsnToWrite, 3> newInsns;
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
    llvm_unreachable("unknown transformation");
  }

  uint32_t offset = reloc->offset - (relocProperty->getInstSize() == 6 ? 2 : 0);

  // Whether we are inserting a new reloc, or just changing the existing one
  bool newReloc = false;
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
        // Note: We push new relocations to the end
        isec->relocations.push_back(newRelocation);
        // Because we add a relocation, it might invalidate our previous reloc
        reloc = &isec->relocations[relNum];
      }
    }

    newInsns.emplace_back(newInsn, offset, insTemplate.getSize());
    LLVM_DEBUG(llvm::dbgs() << "New instruction " << insTemplate.getName()
                            << ": 0x" << utohexstr(newInsn) << " to offset: 0x"
                            << utohexstr(offset) << "\n");
    offset += insTemplate.getSize();
  }

  return newInsns;
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
    return nullptr;
  }
}

const NanoMipsTransformTemplate *NanoMipsTransformRelax::getTransformTemplate(
    const NanoMipsInsProperty *insProperty, uint32_t relNum,
    uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const {

  const uint32_t bits = config->wordsize * 8;
  uint64_t gpVal = SignExtend64(ElfSym::nanoMipsGp->getVA(0), bits);
  const Relocation &reloc = isec->relocs()[relNum];

  switch (reloc.type) {
  case R_NANOMIPS_PC14_S1: {
    uint64_t val = valueToRelocate - 4;
    uint32_t sReg = insProperty->getSReg(insn);
    if (val != 0 && sReg != 0 && isUInt<5>(val) && ((val & 0x1) == 0))
      return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL16, reloc.type);

    // Adjust value if this is a backward branch
    if (static_cast<int64_t>(val) < 0)
      val += 2;

    if (sReg == 0 && isInt<8>(val) && ((val & 0x1) == 0))
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
    llvm_unreachable("unkown relocation for transformation");
  }
}

// NanoMipsTransformExpand

const NanoMipsInsProperty *lld::elf::NanoMipsTransformExpand::getInsProperty(
    uint64_t insn, uint64_t insnMask, RelType reloc,
    InputSectionBase *isec) const {

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
    const NanoMipsInsProperty *insProperty, uint32_t relNum,
    uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const {

  const Relocation &reloc = isec->relocs()[relNum];
  const uint32_t bits = config->wordsize * 8;
  uint64_t gpVal = SignExtend64(ElfSym::nanoMipsGp->getVA(0), bits);
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
    // gold subtracts 6, but the relocation is pointing at beginning of the
    // instruction + 2
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
    llvm_unreachable("unknown relocation for transformation");
  }
  return getExpandTransformTemplate(insProperty, reloc, insn, isec);
}

const NanoMipsTransformTemplate *
lld::elf::NanoMipsTransformExpand::getExpandTransformTemplate(
    const NanoMipsInsProperty *insProperty, const Relocation &reloc,
    uint64_t insn, const InputSection *isec) const {

  bool nanoMipsFullAbi = isec->nanoMipsRelaxAux->fullNanoMipsISA;
  bool pcrel = isec->nanoMipsRelaxAux->pcrel;
  switch (reloc.type) {
  case R_NANOMIPS_PC21_S1: {
    if (insProperty->getName() == "move.balc")
      // TODO: Generates 16bit move with balc, maybe should
      // be banned with insn32, or generate 32 bit ins to move
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
    llvm_unreachable("unknown relocation for transformation");
  }
}

SmallVector<NewInsnToWrite, 3> NanoMipsTransformExpand::getTransformInsns(
    Relocation *reloc, const NanoMipsTransformTemplate *transformTemplate,
    const NanoMipsInsProperty *insProperty,
    const NanoMipsRelocProperty *relocProperty, InputSection *isec,
    uint64_t insn, uint32_t relNum) const {

  RelType oldRelType = reloc->type;
  auto newInsns = NanoMipsTransform::getTransformInsns(
      reloc, transformTemplate, insProperty, relocProperty, isec, insn, relNum);

  // We are adding a symbol for branch over the bc instruction, as this
  // generates a negative branch + bc32 and the negative branch needs to skip
  // bc32
  if ((oldRelType == R_NANOMIPS_PC14_S1 || oldRelType == R_NANOMIPS_PC11_S1) &&
      transformTemplate->getType() == TT_NANOMIPS_PCREL32_LONG) {
    // We changed the reloc array, so we need to get our reloc from the reloc
    // vector
    reloc = &isec->relocations[relNum];
    // The last reloc added is the new relocation, which is the reloc to bc32
    const Relocation &newReloc = isec->relocations.back();

    Defined *s = addSyntheticLinkerSymbol(Twine(skipBcSymPrefix) +
                                              Twine(newSkipBcSymCount),
                                          newReloc.offset + bc32Size, 0, isec);
    newSkipBcSymCount++;
    reloc->sym = s;
    isec->nanoMipsRelaxAux->anchors.push_back({s, false});
    LLVM_DEBUG(llvm::dbgs()
                   << "New symbol " << s->getName() << " in section "
                   << (isec->file ? isec->file->getName() : "no file") << ":"
                   << isec->name << " on offset " << s->value << "\n";);
  }

  return newInsns;
}

// NanoMipsTransformController

// Helper functions and structs to NanoMipsTransformController

namespace {

inline bool isOutputSecTransformable(const OutputSection *osec) {
  return (osec->flags & (SHF_EXECINSTR | SHF_ALLOC)) ==
             (SHF_EXECINSTR | SHF_ALLOC) &&
         (osec->type & SHT_PROGBITS);
}

template <class ELFT> inline bool isNanoMipsPcRel(const ObjFile<ELFT> *obj) {
  return (obj->getObj().getHeader().e_flags & llvm::ELF::EF_NANOMIPS_PCREL) !=
         0;
}

bool isForcedInsnLength(uint64_t offset, uint32_t relNum, InputSection *isec) {
  for (uint32_t curRelNum = relNum + 1; curRelNum < isec->relocs().size();
       curRelNum++) {
    Relocation &rel = isec->relocs()[curRelNum];
    if (offset != rel.offset)
      break;
    // TODO: insn32 and insn16 shouldn't ban all transformations,
    // this is how it is done in gold
    if (rel.type == R_NANOMIPS_FIXED || rel.type == R_NANOMIPS_INSN32 ||
        rel.type == R_NANOMIPS_INSN16)
      return true;
  }
  return false;
}

template <endianness E>
uint64_t readInsn(ArrayRef<uint8_t> data, uint64_t off, uint32_t insnSize) {
  assert(off + insnSize <= data.size() && "Overflow on buffer in readInsn");
  if (insnSize == 6)
    return read16(&data[off]);
  else if (insnSize == 4)
    return nanoMipsReadShuffle32<E>(&data[off]);
  else if (insnSize == 2)
    return read16(&data[off]);
  else
    llvm_unreachable(
        "Unknown byte size of nanoMIPS instruction (only 2, 4 and 6 known)");
}

template <endianness E>
void writeInsn(uint64_t insn, ArrayRef<uint8_t> data, uint64_t off,
               uint32_t insnSize) {
  assert(off + insnSize <= data.size() && "Overflow on buffer in writeInsn");
  uint8_t *dataPtr = const_cast<uint8_t *>(data.begin());
  if (insnSize == 6)
    write16(dataPtr + off, (uint16_t)insn);
  else if (insnSize == 4)
    nanoMipsWriteShuffle32<E>(dataPtr + off, ((uint32_t)insn));
  else if (insnSize == 2)
    write16(dataPtr + off, (uint16_t)insn);
  else
    llvm_unreachable(
        "Unknown byte size of nanoMIPS instruction (only 2, 4, and 6 known)");
}
} // namespace

template <class ELFT> void NanoMipsTransformController<ELFT>::initState() {
  if (config->relax)
    this->currentState = &this->transformRelax;
  else if (config->expand)
    this->currentState = &this->transformExpand;
  else
    this->currentState = &this->transformNone;

  LLVM_DEBUG(llvm::dbgs() << "Initial state of transform: "
                          << this->currentState->getTypeAsString() << "\n");

  return;
}

template <class ELFT>
void NanoMipsTransformController<ELFT>::changeState(int pass) {

  if (this->currentState->getChangedThisIteration()) {
    // We want to repeat the transformation until it doesn't change anything in
    // iterations
    this->currentState->resetChangedThisIteration();
    return;
  }

  if ((notExpandedYet || this->currentState->getChanged()) &&
      (this->currentState->getType() == NanoMipsTransform::TransformRelax) &&
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

// TODO: Emit reloc option
// relaxOnce is used for both relaxations and expansions
template <class ELFT>
bool NanoMipsTransformController<ELFT>::relaxOnce(int pass) const {
  if (this->isNone())
    return false;
  LLVM_DEBUG(llvm::dbgs() << "Transformation Pass num: " << pass << "\n";);
  bool shouldRunAgain = false;
  if (this->mayRelax()) {
    if (pass == 0) {
      // Initialization of additional info that are needed for
      // relaxations/expansions
      initTransformAuxInfo();
    }
    for (OutputSection *osec : outputSections) {
      if (!isOutputSecTransformable(osec))
        continue;

      SmallVector<InputSection *, 0> storage;
      for (InputSection *sec : getInputSections(*osec, storage)) {
        if (!this->safeToModify(sec))
          continue;
        if (sec->relocs().size())
          this->scanAndTransform(sec);
      }
    }

    const_cast<NanoMipsTransformController<ELFT> *>(this)->changeState(pass);
    if (!this->isNone())
      shouldRunAgain = true;
  }

  return shouldRunAgain;
}

template <class ELFT>
inline bool lld::elf::NanoMipsTransformController<ELFT>::safeToModify(
    InputSection *sec) const {
  bool modifiable = false;
  if (auto *obj = sec->getFile<ELF32LE>()) {
    modifiable =
        (obj->getObj().getHeader().e_flags & EF_NANOMIPS_LINKRELAX) != 0;
  }
  return modifiable;
}

template <class ELFT>
void NanoMipsTransformController<ELFT>::initTransformAuxInfo() const {
  SmallVector<InputSection *, 0> storage;
  for (OutputSection *osec : outputSections) {
    if (!isOutputSecTransformable(osec))
      continue;

    for (InputSection *sec : getInputSections(*osec, storage)) {
      if (!this->safeToModify(sec) || sec->relocs().size() == 0)
        continue;
      sec->nanoMipsRelaxAux = make<NanoMipsRelaxAux>();
      sec->nanoMipsRelaxAux->isAlreadyTransformed = false;
      sec->nanoMipsRelaxAux->freeBytes = 0;

      auto *abiFlagsSec = NanoMipsAbiFlagsSection<ELFT>::get();

      auto *obj = sec->getFile<ELFT>();

      sec->nanoMipsRelaxAux->pcrel = obj ? isNanoMipsPcRel<ELFT>(obj) : true;

      sec->nanoMipsRelaxAux->fullNanoMipsISA = false;
      if (!abiFlagsSec)
        error("Abi flags section not created, it is needed to determine "
              "whether full nanoMIPS ISA is used!");
      else
        sec->nanoMipsRelaxAux->fullNanoMipsISA =
            abiFlagsSec->isFullNanoMipsISA(sec);
    }
  }

  DenseSet<Defined *> seenWrapped;
  for (InputFile *file : ctx.objectFiles) {
    for (Symbol *sym : file->getSymbols()) {
      auto *d = dyn_cast<Defined>(sym);
      if (!d || d->file != file)
        continue;
      if (auto *sec = dyn_cast_or_null<InputSection>(d->section)) {
        if ((sec->flags & (SHF_EXECINSTR | SHF_ALLOC)) !=
                (SHF_EXECINSTR | SHF_ALLOC) ||
            !(sec->type & SHT_PROGBITS) || !sec->nanoMipsRelaxAux)
          continue;
        // Note: Wrapped symbols change their real symbol, and we
        // get two pointers to the same wrap symbol, which we don't
        // need in the Aux struct as we are going to update that
        // symbol twice then.
        if (d->getName().starts_with("__wrap_"))
          if (!seenWrapped.insert(d).second)
            continue;
        sec->nanoMipsRelaxAux->anchors.push_back({d, false});
        if (d->size > 0)
          sec->nanoMipsRelaxAux->anchors.push_back({d, true});
      }
    }
  }
}

template <class ELFT>
void NanoMipsTransformController<ELFT>::scanAndTransform(
    InputSection *sec) const {

  LLVM_DEBUG(llvm::dbgs() << "\nStarted transforming " << sec->name
                          << " section\n\n");
  bool seenNoRelax = false;

  const uint32_t bits = config->wordsize * 8;
  uint64_t secAddr = sec->getOutputSection()->addr + sec->outSecOff;
  // Need to traverse relocations this way because at transform we may
  // invalidate the iterator and add some relocs
  for (uint32_t relNum = 0; relNum < sec->relocs().size(); relNum++) {
    Relocation &reloc = sec->relocs()[relNum];

    // TODO: Check out if relocation with the same offset as R_NANOMIPS_NORELAX
    // should or shouldn't be relaxed, NORELAX and RELAX are put after the
    // relocation referring to the instruction by the gnu assembler, as are
    // FIXED, INSN32, INSN16...
    if (reloc.type == R_NANOMIPS_NORELAX) {
      seenNoRelax = true;
      continue;
    }

    if (reloc.type == R_NANOMIPS_RELAX) {
      seenNoRelax = false;
      continue;
    }

    if (seenNoRelax)
      continue;

    if (reloc.type == R_NANOMIPS_ALIGN) {
      this->align(sec, reloc, relNum);
      continue;
    }

    const NanoMipsRelocProperty *relocProp =
        relocPropertyTable.getRelocProperty(reloc.type);
    if (!relocProp)
      continue;

    uint32_t instSize = relocProp->getInstSize();

    if (instSize == 0)
      continue;

    // 48 bit instruction reloc offsets point to 32 bit imm/off not to the
    // beginning of ins
    uint32_t relocOffset = reloc.offset - (instSize == 6 ? 2 : 0);
    uint64_t insn =
        readInsn<ELFT::TargetEndianness>(sec->content(), relocOffset, instSize);

    uint64_t insMask = relocProp->getMask();
    LLVM_DEBUG(
        llvm::dbgs() << "Reloc property: " << relocProp->getName() << "\n";
        llvm::dbgs() << "\tInsMask: 0x" << utohexstr(insMask) << "\n";
        llvm::dbgs() << "Instruction Read: 0x" << utohexstr(insn) << "\n";);

    const NanoMipsInsProperty *insProperty =
        currentState->getInsProperty(insn, insMask, reloc.type, sec);
    if (!insProperty)
      continue;

    if (isForcedInsnLength(relocOffset, relNum, sec))
      continue;

    LLVM_DEBUG(llvm::dbgs()
                   << "InsProperty: " << insProperty->toString() << "\n";);

    // Ignore undef weak symbols
    if (reloc.sym && reloc.sym->isUndefWeak())
      continue;

    uint64_t addrLoc = secAddr + reloc.offset;
    uint64_t valueToRelocate = llvm::SignExtend64(
        sec->getRelocTargetVA(sec->file, reloc.type, reloc.addend, addrLoc,
                              *reloc.sym, reloc.expr),
        bits);

    const NanoMipsTransformTemplate *transformTemplate =
        currentState->getTransformTemplate(insProperty, relNum, valueToRelocate,
                                           insn, sec);

    if (!transformTemplate)
      continue;
    LLVM_DEBUG(llvm::dbgs() << "Chosen transform template:\n"
                            << transformTemplate->toString() << "\n";);

    // Bytes to remove/add
    int32_t delta = transformTemplate->getSizeOfTransform() - instSize;

    currentState->updateSectionContent(sec, relocOffset + instSize, delta);

    // Transform
    // Note: Reloc may be invalidated, but we don't need it from this point on
    // To restore it use its relNum, not the one after transform as it changes
    auto newInsns = currentState->getTransformInsns(
        &reloc, transformTemplate, insProperty, relocProp, sec, insn, relNum);

    for (auto &newInsn : newInsns) {
      writeInsn<ELFT::TargetEndianness>(newInsn.insn, sec->content(),
                                        newInsn.offset, newInsn.size);
    }
    newInsns.clear();
  }
  return;
}

template <class ELFT>
void lld::elf::NanoMipsTransformController<ELFT>::align(InputSection *sec,
                                                        Relocation &reloc,
                                                        uint32_t relNum) const {

  assert(isa<Defined>(reloc.sym) &&
         "R_NANOMIPS_ALIGN should refer to a defined symbol");

  // TODO: Find out how to specify fill size and max in tests, as fill size is
  // always 1 byte, and max cannot even be specified for align
  const uint32_t nop32 = 0x8000c000;
  const uint32_t nop16 = 0x9008;

  // alignment is kept in symbol's value, as an exponent of 2
  uint64_t align = 1 << reloc.sym->getVA();
  uint64_t addr = sec->getOutputSection()->addr + sec->outSecOff + reloc.offset;
  // Note: the reinterpret cast is safe here, as in alignAddr function it is
  // also used to change the pointer to an unsigned long
  uint64_t newAddr = alignAddr(reinterpret_cast<void *>(addr), Align(align));

  uint64_t newPadding = newAddr - addr;
  uint64_t oldPadding = reloc.sym->getSize();

  uint64_t fill = nop16;
  uint64_t max = ELFT::Is64Bits ? (uint64_t)(0) - 1 : (uint32_t)(0) - 1;
  size_t fillSize = 2;

  for (uint32_t i = relNum + 1; i < sec->relocs().size(); i++) {
    Relocation &r = sec->relocs()[i];
    if (r.offset != reloc.offset)
      break;

    if (r.type == R_NANOMIPS_FILL) {
      fill = r.sym->getVA();
      fillSize = cast<Defined>(r.sym)->size;
    } else if (r.type == R_NANOMIPS_MAX) {
      max = r.sym->getVA();
    }
  }

  // Set the padding to 0, if the padding bytes exceed max bytes
  if (newPadding > max)
    newPadding = 0;

  // Equal paddings, mean nothing should change, so return
  if (newPadding == oldPadding)
    return;

  int64_t count = (int64_t)(newPadding - oldPadding);

  // Check if we are cutting nop32 on half, then we need
  // to replace it with nop16 instruction
  if (count < 0 && newPadding >= 2) {
    uint64_t insn = readInsn<ELFT::TargetEndianness>(
        sec->content(), reloc.offset + newPadding - 2, 4);
    if (insn == nop32) {
      writeInsn<ELFT::TargetEndianness>(nop16, sec->content(),
                                        reloc.offset + newPadding - 2, 2);
      LLVM_DEBUG(llvm::dbgs()
                     << "nop[32] is replaced with nop[16] due to new alignment "
                        "on offset "
                     << reloc.offset + newPadding - 2 << " in section "
                     << sec->name << " from obj "
                     << (sec->file ? sec->file->getName() : "None") << "\n";);
    }
  }

  currentState->updateSectionContent(sec, reloc.offset + oldPadding, count,
                                     true);

  // Update size of symbol
  cast<Defined>(reloc.sym)->size = newPadding;

  // Add padding
  if (count > 0) {
    if (fillSize > static_cast<uint64_t>(count)) {
      fill = nop16;
      fillSize = 2;
    }

    for (int i = 0; i < count; i += fillSize) {
      // This shouldn't really happen among instructions
      if (LLVM_UNLIKELY(fillSize == 1))
        write8(const_cast<uint8_t *>(sec->content().begin()) + reloc.offset +
                   oldPadding + i,
               fill);
      else
        writeInsn<ELFT::TargetEndianness>(
            fill, sec->content(), reloc.offset + oldPadding + i, fillSize);
    }
  }
}

template class lld::elf::NanoMipsTransformController<ELF32LE>;
template class lld::elf::NanoMipsTransformController<ELF32BE>;
template class lld::elf::NanoMipsTransformController<ELF64LE>;
template class lld::elf::NanoMipsTransformController<ELF64BE>;