//===- NanoMipsProperties.h -----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLD_ELF_NANOMIPS_PROPERTIES_H
#define LLD_ELF_NANOMIPS_PROPERTIES_H

#include <string>
#include "Relocations.h"
#include "llvm/ADT/SmallSet.h"

#undef TRANSFORM_ENUM
#define TRANSFORM_ENUM

#include "NanoMipsRelocInsProperty.inc"

#undef TRANSFORM_ENUM

// using namespace llvm;
// using namespace lld;
// Used for relaxation purposes

namespace lld {
namespace elf{
  class NanoMipsRelocProperty;
  // Should be used only by NanoMips target, contains NanoMips reloc properties
    class NanoMipsRelocPropertyTable final {
    public:
     ~NanoMipsRelocPropertyTable();
      NanoMipsRelocPropertyTable();   
      // Debug only
      std::string toString() const;

      const NanoMipsRelocProperty *getRelocProperty(RelType rel) const;

    private:
      friend class NanoMips;

      NanoMipsRelocPropertyTable(NanoMipsRelocPropertyTable &p) = delete;
      void operator=(const NanoMipsRelocPropertyTable &p) = delete;
      static const uint32_t RelocPropertyTableSize = 256;
      NanoMipsRelocProperty *table[RelocPropertyTableSize];
  };

  // Used for relaxation purposes
  // Properties of relocations, mask is used to recognize the instructions that this relocation refers to.
  class NanoMipsRelocProperty final {
    public:
      // Debug only 
      std::string toString() const;
      StringRef getName() const { return name; }
      uint32_t getNumOfBitsToRelocate() const { return bitsToRelocate; }
      uint32_t getInstSize() const { return instSize; }
      uint64_t getMask() const { return mask; }

    private:
      friend class NanoMipsRelocPropertyTable;
      NanoMipsRelocProperty(NanoMipsRelocProperty &p) = delete;
      void operator=(const NanoMipsRelocProperty &p) = delete;
      NanoMipsRelocProperty(std::string Name, uint32_t InstSize, uint32_t BitsToRelocate, uint64_t Mask) 
      : name(Name), bitsToRelocate(BitsToRelocate), instSize(InstSize), mask(Mask) {}
      std::string name;
      uint32_t bitsToRelocate;
      // Size is in bytes
      uint32_t instSize;
      uint64_t mask;
  };

  class NanoMipsInsTemplate final {
    public:
      // Debug only
      std::string toString() const;
      uint32_t getSize() const { return size; }
      // function pointer for inserting registers in Instruction Templates
      using InsertRegFun = uint64_t (*)(uint32_t, uint32_t, uint64_t);

      uint64_t getInstruction(uint32_t treg, uint32_t sreg) const;

    private:
      friend class NanoMipsInsPropertyTable;
      // private may only be used by NanoMipsPropertyTable, to create them with transform templates and ins properties
      NanoMipsInsTemplate(const char *name, uint64_t data, RelType reloc, uint32_t size, InsertRegFun insertTReg, InsertRegFun insertSReg)
      : name(name), data(data), reloc(reloc), size(size), insertTReg(insertTReg), insertSReg(insertSReg) {}

      // ins name
      std::string name;
      // altough we use only up to 4 bytes of data
      // I chose uint64_t just as a precaution
      // This is data with opcode, we use this to insert
      // registers to it
      uint64_t data;
      RelType reloc;
      // size is in bytes, attention to the fact
      // that only first 2 bytes of 48bit ins
      // contatin non offset or imm data, so
      // we only use those two in NanoMips
      uint32_t size; 
      InsertRegFun insertTReg;
      InsertRegFun insertSReg;
  };

  // Currently making structures to use macros
  class NanoMipsTransformTemplate final {
    public:
    // Relocs will be statically allocated so no need for deleter 
    // Debug only
    std::string toString() const;
    private:
    friend class NanoMipsInsPropertyTable;
    // Only ins property table can build this this
    NanoMipsTransformTemplate(const NanoMipsInsTemplate *insns, uint32_t insCount, const RelType *relocs, uint32_t relocCount)
      : insns(insns), insCount(insCount), relocs(relocs), relocCount(relocCount) 
      {
        totalSizeOfTransform = 0;
        for(uint32_t i = 0; i < insCount; i++) totalSizeOfTransform += insns[i].getSize();
      }
    
    // Copying not allowed
    NanoMipsTransformTemplate(const NanoMipsTransformTemplate &) = delete;
    void operator=(const NanoMipsTransformTemplate &) = delete;

    // Instructions that compose this transformation
    const NanoMipsInsTemplate *insns;
    // Num of instructions that compose this transformation
    uint32_t insCount;
    // Relocs for which we are using this transformation
    const RelType *relocs;
    uint32_t relocCount;
    // Size of transformation in bytes
    uint32_t totalSizeOfTransform;



  };

  class NanoMipsInsProperty final {
    public:
      using ExtractRegFun = uint32_t (*)(uint64_t);
      using ConvertRegFun = uint32_t (*)(uint32_t);
      using IsValidRegFun = bool (*)(uint32_t);
      // For debug only
      std::string toString() const;
      // We will probably need a destructor for NanoMipsTransformTemplates
      ~NanoMipsInsProperty();
    private:
      friend class NanoMipsInsPropertyTable;
      // Only accessible by NanoMipsInsPropertyTable, used to initialize transformations in property
      void addTransform(const NanoMipsTransformTemplate *transformTemplate, NanoMipsTransformType type, const RelType *relocs, uint32_t relocCount);
      NanoMipsInsProperty(const char *name, ExtractRegFun extractTReg, ConvertRegFun convertTReg, IsValidRegFun isValidTReg, ExtractRegFun extractSReg, ConvertRegFun convertSReg, IsValidRegFun isValidSReg)
      :name(name), extractTReg(extractTReg), convertTReg(convertTReg), isValidTReg(isValidTReg), extractSReg(extractSReg), convertSReg(convertSReg), isValidSReg(isValidSReg){}

      // Copying not allowed
      NanoMipsInsProperty(const NanoMipsInsProperty &) = delete;
      void operator=(const NanoMipsInsProperty &) = delete;

      std::string name;
      // Map of transformations for the given instruction
      // Checkout llvm's datatypes to change this
      // I think 17 is max num of transform templates, but most of ins props have less or equal to 8
      // NanoMipsTransformType is the key, but SmallDenseMap has implementation for uint32_t keys
      llvm::SmallDenseMap<uint32_t, const NanoMipsTransformTemplate *, 8> transformationMap;
      // std::map<NanoMipsTransformType, const NanoMipsTransformTemplate *> transformationMap;
      llvm::SmallSet<RelType, 8> relocs;

      ExtractRegFun extractTReg;
      ConvertRegFun convertTReg;
      IsValidRegFun isValidTReg;

      ExtractRegFun extractSReg;
      ConvertRegFun convertSReg;
      IsValidRegFun isValidSReg;
  };

  class NanoMipsInsPropertyTable final {
    public:
      NanoMipsInsPropertyTable();
      ~NanoMipsInsPropertyTable();
      // For debugging only
      std::string toString() const;
      // We will need a destructor as well
    private:

      // Copying not allowed
      NanoMipsInsPropertyTable(const NanoMipsInsPropertyTable &) = delete;
      void operator=(const NanoMipsInsPropertyTable &) = delete;

      llvm::DenseMap<uint32_t, NanoMipsInsProperty *> insMap;

  };

}
}



#endif