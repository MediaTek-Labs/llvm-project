//===- NanoMipsProperties.cpp -----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "NanoMipsProperties.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringExtras.h"
#include "Config.h"
#include "SyntheticSections.h"
#include "llvm/Object/ELFTypes.h"
#include "Symbols.h"
#include "OutputSections.h"
#include "llvm/Support/Process.h"
#include "lld/Common/Memory.h"

#include "Target.h"


#define DEBUG_TYPE "lld-nanomips"
using namespace lld;
using namespace lld::elf;
using namespace llvm;
using namespace llvm::ELF;
using namespace llvm::object;
using namespace llvm::support::endian;

// Function templates for insert, extract, convert and isValid Reg

enum NanoMipsRegisterMapping{
  RM_NANOMIPS_TREG,
  RM_NANOMIPS_SREG
};

template<uint32_t Pos, uint32_t Size, NanoMipsRegisterMapping REG>
uint64_t insertReg(uint32_t treg, uint32_t sreg, uint64_t data)
{
  uint32_t reg;
  // C++ 17
  // if constexpr(REG == RM_NANOMIPS_TREG) reg = treg;
  // else reg = sreg;

  if(REG == RM_NANOMIPS_TREG) reg = treg;
  else reg = sreg;

  return data | ((reg & ((1U << Size) - 1)) << Pos);

}

template<uint32_t Pos, uint32_t Size>
uint32_t extractReg(uint64_t data)
{
  return (data >> Pos) & ((1U << Size) - 1);
}

uint32_t moveBalcTReg(uint64_t data)
{
  uint32_t reg = ((data >> 21) & 0x7) | ((data >> 22) & 0x8);
  static uint32_t gpr4ZeroMap[] = { 8, 9, 10, 0, 4, 5, 6, 7, 16, 17, 18, 19, 20, 21, 22, 23 };
  return gpr4ZeroMap[reg];
}

// We'll call it SReg even though it is a DReg bc every ins has a TReg and a SReg in ins properties
uint32_t moveBalcSReg(uint64_t data)
{
  return ((data >> 24) & 0x1) + 4;
}

// Convert 3-bit to 5-bit reg
uint32_t convertReg(uint32_t reg)
{
  static uint32_t gpr3Map[] = { 16, 17, 18, 19, 4, 5, 6, 7 };
  assert(reg < sizeof(gpr3Map) / sizeof(gpr3Map[0]));
  return gpr3Map[reg];
}

// Check if a 5-bit reg can be converted to a 3-bit one
bool isRegValid(uint32_t reg)
{
  return ((4 <= reg && reg <= 7) || (16 <= reg && reg <= 19));
}

// NanoMipsRelocPropertyTable

NanoMipsRelocPropertyTable::NanoMipsRelocPropertyTable()
{
  for(uint32_t i = 0; i < RelocPropertyTableSize; i++) table[i] = nullptr;
  #undef RELOC_PROPERTY
  #define RELOC_PROPERTY(relocName, instSize, bitsToRelocate, mask) \
  { \
   RelType type = R_NANOMIPS_##relocName; \
   assert(type < RelocPropertyTableSize && "Relocations mustn't be larger than 256"); \
   this->table[type] = new NanoMipsRelocProperty("R_NANOMIPS_"#relocName, instSize / 8, bitsToRelocate, mask); \
  }
  #include "NanoMipsRelocInsProperty.inc"
  #undef RELOC_PROPERTY
}

const NanoMipsRelocProperty *NanoMipsRelocPropertyTable::getRelocProperty(RelType rel) const {
  assert(rel < RelocPropertyTableSize);
  return table[rel];
}

NanoMipsRelocPropertyTable::~NanoMipsRelocPropertyTable()
{
  // C++ 14
  llvm::for_each(table, [](auto& elem){delete elem; elem = nullptr;});
  // for(uint32_t i = 0; i < RelocPropertyTableSize; i++){
  //   if(table[i] != nullptr)
  //   {
  //     delete table[i];
  //     table[i] = nullptr;
  //   }
  // }
}

std::string NanoMipsRelocPropertyTable::toString() const
{
  std::string tmp;
  raw_string_ostream SS(tmp);
  for(uint32_t i = 0; i < RelocPropertyTableSize; i++)
  {
    if(table[i])
    {
      SS << i
      << "\t" 
      << table[i]->toString()
      << "\n";
    }
  }
  return SS.str();
}

// NanoMipsInsTemplate

std::string NanoMipsInsTemplate::toString() const {
  std::string tmp;
  raw_string_ostream SS(tmp);
  SS << name 
  << ", " <<  utohexstr(data)
  << ", " << reloc
  << ", " << size
  << ", " << (insertTReg != nullptr ? 1 : 0)
  << ", " << (insertSReg != nullptr ? 1 : 0);
  return SS.str();
}

uint64_t NanoMipsInsTemplate::getInstruction(uint32_t tReg, uint32_t sReg) const
{
  uint64_t insData = data;
  if(insertTReg != nullptr)
    insData = insertTReg(tReg, sReg, insData);
  if(insertSReg != nullptr)
    insData = insertSReg(tReg, sReg, insData);
  return insData;
}

// NanoMipsTransformTemplate

std::string NanoMipsTransformTemplate::toString() const
{
  std::string tmp;
  raw_string_ostream SS(tmp);
  SS << "\t\t\tIns count: " << insCount << "\n";
  for(uint32_t i = 0; i < insCount; i++)
  {
    SS << "\t\t\t\t" << insns[i].toString() << "\n";
  }

  SS << "\t\t\tReloc count: " << relocCount << "\n";
  for(uint32_t i = 0; i < relocCount; i++)
  {
    SS << "\t\t\t\t" << relocs[i] << "\n";
  }

  SS << "\t\t\tTotal size: " << totalSizeOfTransform << "\n";
  return SS.str();
}

// NanoMipsRelocProperty

std::string NanoMipsRelocProperty::toString() const
{
  std::string tmp;
  raw_string_ostream SS(tmp);
  SS << name
  << "\t"
  << bitsToRelocate
  << "\t"
  << instSize
  << "\t"
  << utohexstr(mask);
  return SS.str();
}

// NanoMipsInsProperty

void NanoMipsInsProperty::addTransform(const NanoMipsTransformTemplate *transformTemplate, NanoMipsTransformType type, const RelType *relocs, uint32_t relocCount)
{
  // No checks implemented if everything is good
  for(uint32_t i = 0; i < relocCount; i++)
      this->relocs.insert(relocs[i]);

  transformationMap[type] = transformTemplate;
}

std::string NanoMipsInsProperty::toString() const 
{
  std::string tmp;
  raw_string_ostream SS(tmp);
  SS << name
  << ", extractTReg: "
  << (extractTReg != nullptr ? 1 : 0)
  << ", extractSReg: "
  << (extractSReg != nullptr ? 1 : 0)
  << ", convertTReg: "
  << (convertTReg != nullptr ? 1 : 0)
  << ", convertSReg: "
  << (convertSReg != nullptr ? 1 : 0)
  << ", isValidTReg: "
  << (isValidTReg != nullptr ? 1 : 0)
  << ", isValidSReg: "
  << (isValidSReg != nullptr ? 1 : 0);

  SS << "\n\t\tTransforms:";
  for(auto it = transformationMap.begin(); it != transformationMap.end(); it++)
  {
    uint32_t type = it->first;
    const NanoMipsTransformTemplate *transformTemplate = it->second;
    SS << "\n\t\t\t" << type << "\n" << transformTemplate->toString();
  }

  SS << "\n\t\tRelocs:";
  for(RelType rel : relocs)
  {
    SS << "\n\t\t\t" << rel << "\n";
  }
  return SS.str();
}

NanoMipsInsProperty::~NanoMipsInsProperty()
{
  // C++ 14
  llvm::for_each(transformationMap, [](auto &pair){delete pair.second; pair.second = nullptr;});
  // for(auto it = transformationMap.begin(); it != transformationMap.end(); it++)
  // {
  //   delete it->second;
  //   it->second = nullptr;
  // }
}

// NanoMipsInsPropertyTable

NanoMipsInsPropertyTable::NanoMipsInsPropertyTable()
{
  #undef EXTRACT_REG
  #undef INS_PROPERTY
  #define EXTRACT_REG(Pos, Size) extractReg<Pos, Size>
  #define INS_PROPERTY(name, opcode, extractTReg, convertTReg, isValidTReg, extractSReg, convertSReg, isValidSReg) \
  {\
   NanoMipsInsProperty *insProp = new NanoMipsInsProperty(name, extractTReg, convertTReg, isValidTReg, extractSReg, convertSReg, isValidSReg); \
   insMap[opcode] = insProp; \
   assert(insMap.count(opcode)); \
  }

  #include "NanoMipsRelocInsProperty.inc"
  
  #undef INS_PROPERTY
  #undef EXTRACT_REG

  #undef INSERT_REG
  #undef INS_TEMPLATE
  #undef TRANSFORM_TEMPLATE
  #define INSERT_REG(Pos, Size, Reg) insertReg<Pos, Size, RM_NANOMIPS_##Reg>
  #define INS_TEMPLATE(name, opcode, reloc, insSize, insertTReg, insertSReg) \
  NanoMipsInsTemplate(name, opcode, R_NANOMIPS_##reloc, insSize, insertTReg, insertSReg)
  #define TRANSFORM_TEMPLATE(type, opcode, relocs, insTemplates) \
  { \
    static const NanoMipsInsTemplate insTemplateArray[] = insTemplates; \
    static const RelType relocArray[] = relocs; \
    uint32_t insCount = sizeof(insTemplateArray) / sizeof(NanoMipsInsTemplate);\
    uint32_t relocCount = sizeof(relocArray) / sizeof(RelType);\
    NanoMipsTransformTemplate *transformTemplate = new NanoMipsTransformTemplate(type, insTemplateArray, insCount, relocArray, relocCount); \
    assert(insMap.count(opcode)); \
    insMap[opcode]->addTransform(transformTemplate, type, relocArray, relocCount);\
  }

  #include "NanoMipsRelocInsProperty.inc"

  #undef TRANSOFRM_TEMPLATE
  #undef INS_TEMPLATE
  #undef INSERT_REG
}

std::string NanoMipsInsPropertyTable::toString() const
{
  std::string tmp;
  raw_string_ostream SS(tmp);
  for(auto it = insMap.begin(); it != insMap.end(); it++)
  {
    uint32_t opcode = it->first;
    const NanoMipsInsProperty *insProp = it->second;
    SS << "Opcode: " 
    << utohexstr(opcode)
    << ":\n\t"
    << insProp->toString()
    << "\n";
  }
  return SS.str();
}

NanoMipsInsPropertyTable::~NanoMipsInsPropertyTable()
{
  // C++ 14
  llvm::for_each(insMap, [](auto& pair){delete pair.second; pair.second = nullptr;});
  // for(auto it = insMap.begin(); it != insMap.end(); it++)
  // {
  //   delete it->second;
  //   it->second = nullptr;
  // }
}

NanoMipsInsProperty *NanoMipsInsPropertyTable::findInsProperty(uint64_t insn, uint64_t mask, RelType reloc) const {
  NanoMipsInsProperty *insProperty = this->insMap.lookup(insn & mask);
  if(insProperty && insProperty->hasReloc(reloc)) return insProperty;
  else return nullptr;
}

// NanoMipsTransform

std::string NanoMipsTransform::getTypeAsString() const
{
  switch(this->getType())
  {
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

void NanoMipsTransform::updateSectionContent(InputSection *isec, uint64_t location, int32_t delta){
  // FIXME: Don't like this, when allocating large object files
  // mmap is used with only read rights but we need write as well
  // so we are allocating memory for that file again
  // Note: not using the reallocation, just changed priveleges from
  // readonly to priveleged, this is not good as well should be changed

  // uint32_t PageSize = sys::Process::getPageSizeEstimate();
  // auto * objFile = isec->getFile<ELF32LE>();
  // uint64_t FileSize = objFile->getObj().getBufSize();

  // Taken from shouldUseMmap in MemoryBuffer.cpp
  // FIXME: This doesn't work as is now, find another way
  // if(FileSize >= 4 * 4096 && FileSize >= PageSize && llvm::find(reallocatedFiles, objFile->mb.getBufferIdentifier()) == reallocatedFiles.end())
  // {
  //   const uint8_t *currentObj = objFile->getObj().base();
  //   // TODO: Don't know where this will be deleted? Find out where is the deletion 
  //   char *reallocatedObj = bAlloc.Allocate<char>(FileSize);
  //   memcpy(reallocatedObj, currentObj, FileSize);
  //   StringRef r(const_cast<char *>(reallocatedObj), FileSize);
  //   // FIXME: This may cause trouble somewhere else, but we'll see, as we haven't changed the memory in
  //   // ELF32LEFile
  //   objFile->mb = MemoryBufferRef(r, objFile->mb.getBufferIdentifier());
  //   reallocatedFiles.push_back(objFile->mb.getBufferIdentifier());
  //   for(uint32_t i = 0; i < objFile->getSections().size(); i++)
  //   {
  //     // TODO: See if you should only do this for specific sections
  //     InputSectionBase *sec = objFile->getSections()[i];
  //     auto expectedSec = objFile->getObj().getSection(i);
  //     if(expectedSec)
  //     {
  //       sec->setData(reallocatedObj + expectedSec.get()->sh_offset, expectedSec.get()->sh_size);
  //       LLVM_DEBUG(llvm::dbgs() << "Reallocated sec: " << sec->name << ", as it was allocaed with readonly mmap\n";);
  //     }
  //     else
  //       llvm_unreachable("There should be a section!\n");
  //   }
  //   LLVM_DEBUG(llvm::dbgs() << "Reallocated obj: " << isec->getFile<ELF32LE>()->getName() << ", as it was allocated with readonly mmap\n";);
  // }
  if(delta < 0)
    isec->deleteBytes(location, -delta);
  else
    isec->addBytes(location, delta);

  this->changed = true;
  this->changedThisIteration = true;
  LLVM_DEBUG(llvm::dbgs() << "Changed size of input section " 
              << (isec->getFile<ELF32LE>() ? isec->getFile<ELF32LE>()->getName() : "None") 
              << ":" << isec->name
              << " by " << delta << " on location 0x" << utohexstr(location) << "\n";);

  // TODO: This is not efficient maybe sort relocs by offset, and just traverse  over the ones
  // which have offset larger or equal than the processed one 
  for(auto &reloc: isec->relocations)
  {
    // TODO: Need to make this different for align, fill and max reloc
    // but that will be done after they are implemented
    if(reloc.offset >= location)
      reloc.offset += delta;
  }
  // TODO: See if we should adjust conditional branches? That is a whole new structure

  // Adjust symbols
  // TODO: This is not that efficient, change it to be more efficient

  if(isec->file)
  {
    for(auto *sym: isec->file->symbols)
    {
      if(isa<Defined>(sym))
      {
        auto dSym = cast<Defined>(sym);
        if(sym->getOutputSection() && sym->getOutputSection()->sectionIndex == isec->getOutputSection()->sectionIndex)
        {
          if(dSym->value >= location)
          {
            LLVM_DEBUG(llvm::dbgs() << "Changed symbol value: " << dSym->getName() << " by " << delta << "\n";);
            dSym->value += delta;
          }
          if(dSym->isFunc() && dSym->value < location && dSym->value + dSym->size >= location)
          {
            LLVM_DEBUG(llvm::dbgs() << "Changed symbol size: " << dSym->getName() << " by " << delta << "\n";);
            dSym->size += delta;
          }
        }
      }

    }
  }
}

void NanoMipsTransform::transform(Relocation &reloc, const NanoMipsTransformTemplate *transformTemplate, const NanoMipsInsProperty *insProperty, InputSection *isec, uint64_t insn, uint32_t &relNum) const
{
  uint32_t tReg = 0;
  uint32_t sReg = 0;

  switch(reloc.type)
  {
    case R_NANOMIPS_PC14_S1:
    {
      tReg = insProperty->getTReg(insn);
      sReg = insProperty->getSReg(insn);
      // Check if register swap is necessary
      if(transformTemplate->getType() == TT_NANOMIPS_PCREL16)
      {
        uint32_t tReg16 = tReg & 0x7;
        uint32_t sReg16 = sReg & 0x7;
        if((insProperty->getName() == "beqc" && sReg16 > tReg16)
           || (insProperty->getName() == "bnec" && sReg16 < tReg16))
        {
          uint32_t tmp = tReg;
          tReg = sReg;
          sReg = tmp;
        }
      }
      break;
    }
    case R_NANOMIPS_PC21_S1:
    {
      tReg = insProperty->getTReg(insn);
      if(insProperty->getName() == "move.balc")
        sReg = insProperty->getSReg(insn);
      else
        sReg = tReg;
      break;
    }

    case R_NANOMIPS_PC4_S1:
    {
      tReg = insProperty->convTReg(insn);
      sReg = insProperty->convSReg(insn);
      break;
    }

    default:
    // Should be unreachable, but for now just break and return
      LLVM_DEBUG(llvm::dbgs() << "Transform for relocation: " << reloc.type << " not supported yet!\n");
      return;
  }

  // TODO: Conditional branches here, if added to implementation

  // TODO: Change size for 48-bit insns
  uint32_t offset = reloc.offset;
  
  // Whether we are inserting a new reloc, or just changing the existing one
  bool newReloc = false;
  auto instructionList = makeArrayRef(transformTemplate->getInsns(), transformTemplate->getInsCount());
  for(auto &insTemplate : instructionList)
  {
    uint64_t newInsn = insTemplate.getInstruction(tReg, sReg);
    RelType newRelType = insTemplate.getReloc();

    if(newRelType != R_NANOMIPS_NONE)
    {
      assert(transformTemplate->getType() != TT_NANOMIPS_DISCARD && "There is a reloc for a DISCARD relaxation!");

      uint32_t newROffset = (insTemplate.getSize() == 6 ? offset + 2 : offset);
      if(!newReloc)
      {
        reloc.offset = newROffset;
        reloc.type = newRelType;
        // Only param needed is relType, other ones are not important for nanoMIPS
        reloc.expr = target->getRelExpr(newRelType, *reloc.sym, isec->data().data() + newROffset);
        newReloc = true;
        LLVM_DEBUG(llvm::dbgs() << "Changed current reloc to " << reloc.type << "\n";);
      }
      else
      {
        Relocation newRelocation;
        newRelocation.addend = reloc.addend;
        newRelocation.offset = newROffset;
        // Only param needed is relType, other ones are not important for nanoMIPS
        newRelocation.expr = target->getRelExpr(newRelType, *reloc.sym, isec->data().data() + newROffset);
        newRelocation.sym = reloc.sym;
        newRelocation.type = newRelType;
        isec->relocations.insert(isec->relocations.begin() + relNum, newRelocation);
        relNum++;
        LLVM_DEBUG(llvm::dbgs() << "Added new reloc " << reloc.type << "\n";);
        // TODO: Setting reloc strategy for finalizing relocs
      }
    }


    newInsns.emplace_back(newInsn, offset, insTemplate.getSize());
    LLVM_DEBUG(llvm::dbgs() << "New instruction " << insTemplate.getName() << ": 0x" << utohexstr(newInsn) << " to offset: 0x" << utohexstr(offset) << "\n");
    offset += insTemplate.getSize();
  }

}

uint32_t swap16BitWords(uint32_t val)
{
  uint32_t newLo = val >> 16;
  uint32_t newHi = (val & 0xFFFF) << 16;
  return newHi | newLo;
}

// NanoMipsTransformRelax

const NanoMipsInsProperty *NanoMipsTransformRelax::getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc, InputSectionBase *isec) const
{
  if(config->nanoMipsInsn32)
  {  
    return nullptr;
  }

  const NanoMipsInsProperty *insProperty = this->insPropertyTable->findInsProperty(insn, insnMask, reloc);
  if(!insProperty) return nullptr;

  switch(reloc)
  {
    case R_NANOMIPS_PC14_S1:
    {
      uint32_t sReg = insProperty->getSReg(insn);
      if(!insProperty->hasTransform(TT_NANOMIPS_PCREL16, reloc) 
         || !NanoMipsAbiFlagsSection<ELF32LE>::get()->isFullNanoMipsISA(isec) 
         || (!insProperty->areRegsValid(insn) 
             && !(insProperty->hasTransform(TT_NANOMIPS_PCREL16_ZERO, reloc)
                 && insProperty->isTRegValid(insn)
                 && sReg == 0 
                )  
            )
        )
        return nullptr;
      
      unsigned int tReg = insProperty->getTReg(insn);
      StringRef insName = insProperty->getName();
      // beqc cannot be relaxed to beqc[16] if
      // sReg and tReg are same
      // TODO: Maybe the same needs to be done for bnec
      if((insName == "beqc" || insName == "bnec") && tReg == sReg)
        return nullptr;
      return insProperty;
    }
    case R_NANOMIPS_PC_I32:
    {
      if(insProperty->hasTransform(TT_NANOMIPS_PCREL32, reloc)) return insProperty;
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

const NanoMipsTransformTemplate *NanoMipsTransformRelax::getTransformTemplate(const NanoMipsInsProperty *insProperty, const Relocation &reloc, uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const
{
  switch(reloc.type)
  {
    case R_NANOMIPS_PC14_S1:
    {
      // TODO: Need to check if shortening instruction messed up the range.
      // End of instruction is used for pcrel jumps.
      uint64_t val = valueToRelocate - 4;
      uint32_t sReg = insProperty->getSReg(insn);
      if(val != 0 && sReg != 0 && isUInt<5>(val))
        return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL16, reloc.type);
      else if(sReg == 0 && isInt<8>(val))
        return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL16_ZERO, reloc.type);
      else
        return nullptr;
    }
    default:
      //TODO: Should be unreachable when all relocs are processed
      break;
  }
  return nullptr;
}

// NanoMipsTransformExpand

const NanoMipsInsProperty * lld::elf::NanoMipsTransformExpand::getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc, InputSectionBase * isec) const
{
  // TODO: Relax checks for nanoMips insn32, should that be done here? Check it.
  switch(reloc)
  {
    case R_NANOMIPS_PC21_S1:
    case R_NANOMIPS_PC4_S1:
      return insPropertyTable->findInsProperty(insn, insnMask, reloc);
    default:
      break;
  }

  return nullptr;
}

const NanoMipsTransformTemplate *lld::elf::NanoMipsTransformExpand::getTransformTemplate(const NanoMipsInsProperty * insProperty, const Relocation & reloc, uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const
{
  switch(reloc.type)
  {
    case R_NANOMIPS_PC21_S1:
    {
      uint64_t val = valueToRelocate - 4;
      if(((val & 0x1) == 0) && isInt<22>(val))
        return nullptr;
      break;
    }
    case R_NANOMIPS_PC4_S1:
    {
      uint64_t val = valueToRelocate - 2;
      if(isUInt<5>(val))
        return nullptr;
      break;
    }
    default:
    // TODO: Should be unreachable when all relocs are processed
    LLVM_DEBUG(llvm::dbgs() << "Relocation: " << reloc.type << " not supported yet for expansions\n";);
    return nullptr;
  }
  return getExpandTransformTemplate(insProperty, reloc, insn, isec);
}

const NanoMipsTransformTemplate *lld::elf::NanoMipsTransformExpand::getExpandTransformTemplate(const NanoMipsInsProperty * insProperty, const Relocation & reloc, uint64_t insn, const InputSection * isec) const
{
  // TODO: Test all versions
  bool nanoMipsFullAbi = NanoMipsAbiFlagsSection<ELF32LE>::get()->isFullNanoMipsISA(isec);
  auto *obj = isec->getFile<ELF32LE>();
  bool pcrel = isNanoMipsPcRel<ELF32LE>(obj);
  switch(reloc.type)
  {
    case R_NANOMIPS_PC21_S1:
      if(insProperty->getName() == "move.balc")
        // See how this goes with insn32 option, as this transformation generates 16bit move with balc
        return insProperty->getTransformTemplate(TT_NANOMIPS_PCREL32_LONG, reloc.type);
      else if(nanoMipsFullAbi)
        return pcrel ? 
          insProperty->getTransformTemplate(TT_NANOMIPS_PCREL_NMF, reloc.type) :
          insProperty->getTransformTemplate(TT_NANOMIPS_ABS_NMF, reloc.type);
      else
        return pcrel ?
          insProperty->getTransformTemplate(TT_NANOMIPS_PCREL32_LONG, reloc.type) :
          insProperty->getTransformTemplate(TT_NANOMIPS_ABS32_LONG, reloc.type);
    case R_NANOMIPS_PC4_S1:
      // Equality should be impossible but just in case
      return insProperty->getSReg(insn) >= insProperty->getTReg(insn) ?
        insProperty->getTransformTemplate(TT_NANOMIPS_BNEC32, reloc.type) :
        insProperty->getTransformTemplate(TT_NANOMIPS_BEQC32, reloc.type);
    default:
      // Should be unreachable when all relocs are processed
      LLVM_DEBUG(llvm::dbgs() << "Relocation: " << reloc.type << " not supported yet for expandsions\n";);
      return nullptr;
  }
}

// NanoMipsTransformController

void NanoMipsTransformController::initState()
{
  if(config->relax)
  { 
    this->currentState = &this->transformRelax;
  }
  else if(config->expand) this->currentState = &this->transformExpand;
  else this->currentState = &this->transformNone;

  LLVM_DEBUG(llvm::dbgs() << "Initial state of transform: " << this->currentState->getTypeAsString() << "\n");

  return;
}

void NanoMipsTransformController::changeState()
{
  if(this->currentState->getChangedThisIteration()) 
  {
    // We want to repeat the transformation until it doesn't change anything in iterations
    this->currentState->resetChangedThisIteration();
    return;
  }
  if(this->currentState->getChanged() && this->currentState->getType() == NanoMipsTransform::TransformRelax && config->expand)
  {
    this->currentState->resetChanged();
    this->currentState = &this->transformExpand;
    LLVM_DEBUG(llvm::dbgs() << "Changed transform state to Expand\n");
    return;
  }

  if(this->currentState->getChanged() && this->currentState->getType() == NanoMipsTransform::TransformExpand && config->relax)
  {
    this->currentState->resetChanged();
    this->currentState = &this->transformRelax;
    LLVM_DEBUG(llvm::dbgs() << "Changed transform state to Relax\n");
    return;
  }

  this->currentState->resetChanged();
  this->currentState = &this->transformNone;
  LLVM_DEBUG(llvm::dbgs() << "Changed transform state to None\n";);
}