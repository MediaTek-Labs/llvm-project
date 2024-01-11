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

using namespace lld;
using namespace lld::elf;
using namespace llvm;
using namespace llvm::ELF;

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
  if constexpr(REG == RM_NANOMIPS_TREG) reg = treg;
  else reg = sreg;

  // if(REG == RM_NANOMIPS_TREG) reg = treg;
  // else sreg;

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

uint64_t NanoMipsInsTemplate::getInstruction(uint32_t treg, uint32_t sreg) const
{
  uint64_t insData = data;
  if(insertTReg != nullptr)
    insData = insertTReg(treg, sreg, insData);
  if(insertSReg != nullptr)
    insData = insertSReg(treg, sreg, insData);
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
    NanoMipsTransformTemplate *transformTemplate = new NanoMipsTransformTemplate(insTemplateArray, insCount, relocArray, relocCount); \
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