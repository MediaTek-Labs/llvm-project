//===- NanoMipsProperties.h -----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLD_ELF_NANOMIPS_PROPERTIES_H
#define LLD_ELF_NANOMIPS_PROPERTIES_H

#include "Relocations.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/BinaryFormat/ELF.h"
#include "InputFiles.h"

#undef TRANSFORM_ENUM
#define TRANSFORM_ENUM

#include "NanoMipsRelocInsProperty.inc"

#undef TRANSFORM_ENUM

// using namespace llvm;
// using namespace lld;
// Used for relaxation purposes


namespace lld {
namespace elf{

  template<class ELFT>
  bool isNanoMipsPcRel(const ObjFile<ELFT> *obj)
  {
    return (obj->getObj().getHeader().e_flags & llvm::ELF::EF_NANOMIPS_PCREL) != 0;
  }
  
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

      uint64_t getInstruction(uint32_t tReg, uint32_t sReg) const;
      RelType getReloc() const { return reloc; }
      StringRef getName() const { return name; }

    private:
      friend class NanoMipsInsPropertyTable;
      // private may only be used by NanoMipsPropertyTable, to create them with transform templates and ins properties
      NanoMipsInsTemplate(const char *name, uint64_t data, RelType reloc, uint32_t size, InsertRegFun insertTReg, InsertRegFun insertSReg)
      : name(name), data(data), reloc(reloc), size(size / 8), insertTReg(insertTReg), insertSReg(insertSReg) {}

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
    // TODO: Check if this is good
    bool hasReloc(RelType reloc) const
    { return llvm::find(llvm::ArrayRef<const RelType>(relocs, relocCount), reloc); }
    uint32_t getSizeOfTransform() const { return totalSizeOfTransform; }
    NanoMipsTransformType getType() const { return type; }
    const NanoMipsInsTemplate *getInsns() const { return insns; }
    uint32_t getInsCount() const { return insCount; }
    private:
    friend class NanoMipsInsPropertyTable;
    // Only ins property table can build this this
    NanoMipsTransformTemplate(NanoMipsTransformType type, const NanoMipsInsTemplate *insns, uint32_t insCount, const RelType *relocs, uint32_t relocCount)
      : type(type), insns(insns), insCount(insCount), relocs(relocs), relocCount(relocCount), totalSizeOfTransform(0)
      {
        // TODO: See which one of these two to keep
        llvm::for_each(llvm::ArrayRef<const NanoMipsInsTemplate>(insns, insCount), [&](auto& elem){this->totalSizeOfTransform += elem.getSize();});
        // for(uint32_t i = 0; i < insCount; i++) totalSizeOfTransform += insns[i].getSize();
      }
    
    // Copying not allowed
    NanoMipsTransformTemplate(const NanoMipsTransformTemplate &) = delete;
    void operator=(const NanoMipsTransformTemplate &) = delete;

    NanoMipsTransformType type;
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
      bool hasReloc(RelType reloc) const { return relocs.contains(reloc); };
      bool hasTransform(NanoMipsTransformType type, RelType reloc) const 
      { auto *t = this->transformationMap.lookup(type); return t ? t->hasReloc(reloc) : false; };
      
      const NanoMipsTransformTemplate *getTransformTemplate(NanoMipsTransformType type, RelType reloc) const
      { auto *t = this->transformationMap.lookup(type); return t && t->hasReloc(reloc) ? t : nullptr; }

      // Shouldn't be called for instructions that don't have extract sReg
      uint32_t getSReg(uint64_t insn) const
      { assert(this->extractSReg && "No extractSReg"); return extractSReg(insn);}

      // Shouldn't be called for instructions that don't have extract tReg
      uint32_t getTReg(uint64_t insn) const
      { assert(this->extractTReg && "No extractTReg"); return extractTReg(insn); }

      bool isSRegValid(uint64_t insn) const 
      { assert(this->isValidSReg && "No isValidSReg"); return isValidSReg(getSReg(insn)); }

      bool isTRegValid(uint64_t insn) const
      { assert(this->isValidTReg && "No isValidTReg"); return isValidTReg(getTReg(insn)); }

      bool areRegsValid(uint64_t insn) const
      { return isTRegValid(insn) && isSRegValid(insn); }

      uint32_t convTReg(uint64_t insn) const
      { assert(this->convertTReg && "No convertTReg"); return this->convertTReg(getTReg(insn)); }

      uint32_t convSReg(uint64_t insn) const
      { assert(this->convertSReg && "No convertSReg"); return this->convertSReg(getSReg(insn)); }

      StringRef getName() const { return name; }

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
      // TODO: Change names of these valids, maybe a little ambigous
      IsValidRegFun isValidTReg;

      ExtractRegFun extractSReg;
      ConvertRegFun convertSReg;
      IsValidRegFun isValidSReg;
  };

  class NanoMipsInsPropertyTable final {
    public:
      NanoMipsInsPropertyTable();
      ~NanoMipsInsPropertyTable();
      NanoMipsInsProperty *findInsProperty(uint64_t insn, uint64_t mask, RelType reloc) const;
      // For debugging only
      std::string toString() const;
      // We will need a destructor as well
    private:

      // Copying not allowed
      NanoMipsInsPropertyTable(const NanoMipsInsPropertyTable &) = delete;
      void operator=(const NanoMipsInsPropertyTable &) = delete;

      llvm::DenseMap<uint64_t, NanoMipsInsProperty *> insMap;

  };

  
  struct NewInsnToWrite{
    uint64_t insn;
    // From current input section
    uint32_t offset;
    uint32_t size;

    NewInsnToWrite(uint64_t i, uint32_t off, uint32_t sz) : insn(i), offset(off), size(sz) {}
  };
  enum NanoMipsTransformationEnum {
    NANOMIPS_NONE_STATE,
    NANOMIPS_RELAX_STATE,
    NANOMIPS_EXPAND_STATE
  };
  class NanoMipsTransform {
    public:
      enum TransformKind {
        TransformNone = 0,
        TransformRelax = 1,
        TransformExpand = 2
      };
      virtual TransformKind getType() const = 0;
      NanoMipsTransform(const NanoMipsInsPropertyTable *tbl): insPropertyTable(tbl) {}
      virtual ~NanoMipsTransform() {}
      virtual const NanoMipsInsProperty *getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc, InputSectionBase *isec) const = 0;
      virtual const NanoMipsTransformTemplate *getTransformTemplate(const NanoMipsInsProperty *insProperty, const Relocation &reloc, uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const = 0;
      virtual void updateSectionContent(InputSection *isec, uint64_t location, int32_t delta);
      bool getChanged() { return changed; }
      bool getChangedThisIteration() { return changedThisIteration; }
      void resetChanged() { changed = false; }
      void resetChangedThisIteration() { changedThisIteration = false; }
      // Relnum is changed in transform as it is passed by reference
      virtual void transform(Relocation &reloc, const NanoMipsTransformTemplate *transformTemplate, const NanoMipsInsProperty *insProperty, InputSection *isec, uint64_t insn, uint32_t &relNum) const;
      // const NanoMipsInsProperty *
      // Debugging purposes only
      std::string getTypeAsString() const;
      SmallVector<NewInsnToWrite> &getNewInsns() const
      { return newInsns; }
    protected:
      const NanoMipsInsPropertyTable *insPropertyTable;
      // if the code size has been changed in this state
      bool changedThisIteration;
      // if the code size has been changed during this iteration
      bool changed;
    
    private:
      mutable SmallVector<NewInsnToWrite> newInsns;
  };


  class NanoMipsTransformExpand: public NanoMipsTransform 
  {
    public:
      NanoMipsTransformExpand(const NanoMipsInsPropertyTable *tbl): NanoMipsTransform(tbl) { assert(insPropertyTable); }
      TransformKind getType() const override { return TransformExpand; }
      const NanoMipsInsProperty *getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc, InputSectionBase *isec) const override;
      const NanoMipsTransformTemplate *getTransformTemplate(const NanoMipsInsProperty *insProperty, const Relocation &reloc, uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const override;
    private:
    const NanoMipsTransformTemplate *getExpandTransformTemplate(const NanoMipsInsProperty *insProperty, const Relocation &reloc, uint64_t insn, const InputSection *isec) const;
  };

  class NanoMipsTransformRelax: public NanoMipsTransform {
    public:
      NanoMipsTransformRelax(const NanoMipsInsPropertyTable *tbl): NanoMipsTransform(tbl) 
      { 
        assert(insPropertyTable);
        // This is done so that after first relaxation pass, we do expansion
        // regardless of the pass changing or not changing code size
        this->changed = true;   
      }
      TransformKind getType() const override { return TransformRelax; }
      const NanoMipsInsProperty *getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc, InputSectionBase *isec) const override;
      const NanoMipsTransformTemplate *getTransformTemplate(const NanoMipsInsProperty *insProperty, const Relocation &reloc, uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const override;
  };


  class NanoMipsTransformNone: public NanoMipsTransform {
    public:
      // NanoMipsTransformNone won't use the table so it doesn't need it
      NanoMipsTransformNone(const NanoMipsInsPropertyTable *): NanoMipsTransform(nullptr) {}
      TransformKind getType() const override { return TransformNone; }
      const NanoMipsInsProperty *getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc, InputSectionBase *isec) const override { return nullptr; }
      const NanoMipsTransformTemplate *getTransformTemplate(const NanoMipsInsProperty *insProperty, const Relocation &reloc, uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const override { return nullptr; }

  };

  class NanoMipsTransformController {
    public:
      NanoMipsTransformController(const NanoMipsInsPropertyTable *tbl): transformRelax(tbl), transformExpand(tbl), transformNone(tbl), currentState(&transformNone){}

      void initState();
      void changeState();
      NanoMipsTransform::TransformKind getType() const { return this->currentState->getType(); }
      // should be called before change state
      bool shouldRunAgain() const { return this->currentState->getChanged(); }
      const NanoMipsInsProperty *getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc, InputSectionBase *isec) const 
      { return this->currentState->getInsProperty(insn, insnMask, reloc, isec); }
      const NanoMipsTransformTemplate *getTransformTemplate(const NanoMipsInsProperty *insProperty, const Relocation &reloc, uint64_t valueToRelocate, uint64_t insn, const InputSection *isec) const
      { return this->currentState->getTransformTemplate(insProperty, reloc, valueToRelocate, insn, isec);}

      void updateSectionContent(InputSection *isec, uint64_t location, int32_t delta) const { this->currentState->updateSectionContent(isec, location, delta);}
      void transform(Relocation &reloc, const NanoMipsTransformTemplate *transformTemplate, const NanoMipsInsProperty *insProperty, InputSection *isec, uint64_t insn, uint32_t &relNum) const
      { this->currentState->transform(reloc, transformTemplate, insProperty, isec, insn, relNum); }

      SmallVector<NewInsnToWrite> &getNewInsns() const { return this->currentState->getNewInsns(); }
    private:
      NanoMipsTransformRelax transformRelax;
      NanoMipsTransformExpand transformExpand;
      NanoMipsTransformNone transformNone;
      // This should be declared after transforms
      NanoMipsTransform *currentState;


  };

}
}





#endif