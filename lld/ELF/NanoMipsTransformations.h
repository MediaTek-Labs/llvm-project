//===- NanoMipsTransformations.h
//-----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLD_ELF_NANOMIPS_PROPERTIES_H
#define LLD_ELF_NANOMIPS_PROPERTIES_H

#include "InputFiles.h"
#include "Relocations.h"
#include "Target.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/Endian.h"

using namespace llvm;
using namespace llvm::support;

namespace lld::elf {

#undef TRANSFORM_ENUM
#define TRANSFORM_ENUM

#include "NanoMipsTransformationProperties.inc"

#undef TRANSFORM_ENUM

// Used both in Arch/NanoMips.cpp and NanoMipsTransformations.cpp

template <endianness E> uint32_t nanoMipsReadShuffle32(const uint8_t *loc) {
  // Similar to microMIPS, little endian instructions are encoded as
  // big endian so that the opcode comes first and that the hardware could
  // know sooner if it is a 16bit, 32bit or 48bit instruction
  uint32_t v = read32(loc);
  if (E == support::little)
    return (v << 16) | (v >> 16);
  return v;
}

template <endianness E>
void nanoMipsWriteShuffle32(uint8_t *loc, uint64_t val) {
  uint16_t *words = (uint16_t *)loc;
  if (E == support::little)
    std::swap(words[0], words[1]);

  write32(loc, val);

  if (E == support::little)
    std::swap(words[0], words[1]);
}

struct NanoMipsSymbolAnchor {
  Defined *d;
  bool end;

  NanoMipsSymbolAnchor(Defined *defined, bool e) : d(defined), end(e) {}
};

struct NanoMipsRelaxAux {
  // We'll allocate more bytes during transformations, just in case
  // this field is used to save the number of those extra bytes. (not
  // using dropped bytes as it is uint16_t which is too small some time)
  uint32_t freeBytes;

  // Note: This is needed, as if transformation hasn't happened yet
  // and if mmap is used to map sections from files to memory, than
  // that section will be readonly, thus unviable for transformations.
  // When we transform the section for the first time, we'll copy its content
  // to a newly allocated memory chunk that can be written to as well.
  // Check out: getOpenFileImpl in llvm/lib/Support/MemoryBuffer.cpp
  bool isAlreadyTransformed;
  bool fullNanoMipsISA;
  bool pcrel;
  // Saves symbols from this section for easier access later on
  // Also stores symbol ends, for sized symbols (functions) so
  // its size can be updated
  SmallVector<NanoMipsSymbolAnchor, 0> anchors;
};

class NanoMipsRelocProperty;
class NanoMipsRelocPropertyTable final {
public:
  ~NanoMipsRelocPropertyTable();
  NanoMipsRelocPropertyTable();
  // For debugging
  std::string toString() const;

  const NanoMipsRelocProperty *getRelocProperty(RelType rel) const {
    assert(rel < RelocPropertyTableSize);
    return table[rel];
  }

private:
  friend class NanoMips;

  NanoMipsRelocPropertyTable(NanoMipsRelocPropertyTable &p) = delete;
  void operator=(const NanoMipsRelocPropertyTable &p) = delete;
  static const uint32_t RelocPropertyTableSize = 256;
  NanoMipsRelocProperty *table[RelocPropertyTableSize];
};

// Used for relaxation purposes
// Properties of relocations, mask is used to recognize the instructions that
// this relocation refers to.
class NanoMipsRelocProperty final {
public:
  // For debugging
  std::string toString() const;
  StringRef getName() const { return name; }
  uint32_t getNumOfBitsToRelocate() const { return bitsToRelocate; }
  uint32_t getInstSize() const { return instSize; }
  uint64_t getMask() const { return mask; }

private:
  friend class NanoMipsRelocPropertyTable;
  // Only NanoMipsRelocPropertyTable can build object of this class
  NanoMipsRelocProperty(NanoMipsRelocProperty &p) = delete;
  void operator=(const NanoMipsRelocProperty &p) = delete;
  NanoMipsRelocProperty(std::string Name, uint32_t InstSize,
                        uint32_t BitsToRelocate, uint64_t Mask)
      : name(Name), bitsToRelocate(BitsToRelocate), instSize(InstSize),
        mask(Mask) {}
  std::string name;
  uint32_t bitsToRelocate;
  // Size is in bytes
  uint32_t instSize;
  uint64_t mask;
};

class NanoMipsInsTemplate final {
public:
  // For debugging
  std::string toString() const;
  uint32_t getSize() const { return size; }
  // function pointer for inserting registers in Instruction Templates
  using InsertRegFun = uint64_t (*)(uint32_t, uint32_t, uint64_t);

  uint64_t getInstruction(uint32_t tReg, uint32_t sReg) const;
  RelType getReloc() const { return reloc; }
  StringRef getName() const { return name; }

private:
  friend class NanoMipsInsPropertyTable;
  NanoMipsInsTemplate(const char *name, uint64_t data, RelType reloc,
                      uint32_t size, InsertRegFun insertTReg,
                      InsertRegFun insertSReg)
      : name(name), data(data), reloc(reloc), size(size / 8),
        insertTReg(insertTReg), insertSReg(insertSReg) {}

  // ins name
  std::string name;
  // Bare instruction with opcode, later we add
  // registers values to it
  uint64_t data;
  RelType reloc;
  // size is in bytes
  uint32_t size;
  InsertRegFun insertTReg;
  InsertRegFun insertSReg;
};

class NanoMipsTransformTemplate final {
public:
  // Debug only
  std::string toString() const;
  bool hasReloc(RelType reloc) const {
    return llvm::find(llvm::ArrayRef<const RelType>(relocs, relocCount), reloc);
  }
  uint32_t getSizeOfTransform() const { return totalSizeOfTransform; }
  NanoMipsTransformType getType() const { return type; }
  const NanoMipsInsTemplate *getInsns() const { return insns; }
  uint32_t getInsCount() const { return insCount; }

private:
  friend class NanoMipsInsPropertyTable;
  // Only ins property table can build this
  NanoMipsTransformTemplate(NanoMipsTransformType type,
                            const NanoMipsInsTemplate *insns, uint32_t insCount,
                            const RelType *relocs, uint32_t relocCount)
      : type(type), insns(insns), insCount(insCount), relocs(relocs),
        relocCount(relocCount), totalSizeOfTransform(0) {
    llvm::for_each(
        llvm::ArrayRef<const NanoMipsInsTemplate>(insns, insCount),
        [&](auto &elem) { this->totalSizeOfTransform += elem.getSize(); });
  }

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
  // function pointer for extracting register from Instruction Properties
  using ExtractRegFun = uint32_t (*)(uint64_t);
  // function pointer for converting registers (3-bit to 5-bit)
  using ConvertRegFun = uint32_t (*)(uint32_t);
  // function pointer for checking if the 5-bit register
  // can be converted to a valid 3-bit one
  using IsValidRegFun = bool (*)(uint32_t);
  // For debugging
  std::string toString() const;
  ~NanoMipsInsProperty();
  bool hasReloc(RelType reloc) const { return relocs.contains(reloc); };
  bool hasTransform(NanoMipsTransformType type, RelType reloc) const {
    auto *t = this->transformationMap.lookup(type);
    return t ? t->hasReloc(reloc) : false;
  };

  const NanoMipsTransformTemplate *
  getTransformTemplate(NanoMipsTransformType type, RelType reloc) const {
    auto *t = this->transformationMap.lookup(type);
    return (t && t->hasReloc(reloc)) ? t : nullptr;
  }

  // Shouldn't be called for instructions that don't have extract sReg
  uint32_t getSReg(uint64_t insn) const {
    assert(this->extractSReg && "No extractSReg");
    return extractSReg(insn);
  }

  // Shouldn't be called for instructions that don't have extract tReg
  uint32_t getTReg(uint64_t insn) const {
    assert(this->extractTReg && "No extractTReg");
    return extractTReg(insn);
  }

  bool isSRegValid(uint64_t insn) const {
    assert(this->isValidSReg && "No isValidSReg");
    return isValidSReg(getSReg(insn));
  }

  bool isTRegValid(uint64_t insn) const {
    assert(this->isValidTReg && "No isValidTReg");
    return isValidTReg(getTReg(insn));
  }

  bool areRegsValid(uint64_t insn) const {
    return isTRegValid(insn) && isSRegValid(insn);
  }

  uint32_t convTReg(uint64_t insn) const {
    assert(this->convertTReg && "No convertTReg");
    return this->convertTReg(getTReg(insn));
  }

  uint32_t convSReg(uint64_t insn) const {
    assert(this->convertSReg && "No convertSReg");
    return this->convertSReg(getSReg(insn));
  }

  StringRef getName() const { return name; }

private:
  friend class NanoMipsInsPropertyTable;
  // Only accessible by NanoMipsInsPropertyTable, used to initialize
  // transformations in property
  void addTransform(const NanoMipsTransformTemplate *transformTemplate,
                    NanoMipsTransformType type, const RelType *relocs,
                    uint32_t relocCount);
  // Only ins property table can build this
  NanoMipsInsProperty(const char *name, ExtractRegFun extractTReg,
                      ConvertRegFun convertTReg, IsValidRegFun isValidTReg,
                      ExtractRegFun extractSReg, ConvertRegFun convertSReg,
                      IsValidRegFun isValidSReg)
      : name(name), extractTReg(extractTReg), convertTReg(convertTReg),
        isValidTReg(isValidTReg), extractSReg(extractSReg),
        convertSReg(convertSReg), isValidSReg(isValidSReg) {}

  NanoMipsInsProperty(const NanoMipsInsProperty &) = delete;
  void operator=(const NanoMipsInsProperty &) = delete;

  std::string name;
  // Map of transformations for the given instruction
  // 17 is max num of transform templates, but most of ins props have
  // less or equal to 8.
  // NanoMipsTransformType is the key
  llvm::SmallDenseMap<uint32_t, const NanoMipsTransformTemplate *, 8>
      transformationMap;
  // Relocs from all transform templates
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
  NanoMipsInsProperty *findInsProperty(uint64_t insn, uint64_t mask,
                                       RelType reloc) const;
  // For debugging
  std::string toString() const;

private:
  NanoMipsInsPropertyTable(const NanoMipsInsPropertyTable &) = delete;
  void operator=(const NanoMipsInsPropertyTable &) = delete;

  llvm::DenseMap<uint64_t, NanoMipsInsProperty *> insMap;
};

struct NewInsnToWrite {
  uint64_t insn;
  // From current input section
  uint32_t offset;
  uint32_t size;

  NewInsnToWrite(uint64_t i, uint32_t off, uint32_t sz)
      : insn(i), offset(off), size(sz) {}
};

// A base class for different types of transformations
class NanoMipsTransform {

public:
  enum TransformKind {
    TransformNone = 0,
    TransformRelax = 1,
    TransformExpand = 2
  };
  virtual TransformKind getType() const = 0;
  NanoMipsTransform(const NanoMipsInsPropertyTable *tbl)
      : insPropertyTable(tbl) {}
  virtual ~NanoMipsTransform() {}
  virtual const NanoMipsInsProperty *
  getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc,
                 InputSectionBase *isec) const = 0;
  virtual const NanoMipsTransformTemplate *
  getTransformTemplate(const NanoMipsInsProperty *insProperty, uint32_t relNum,
                       uint64_t valueToRelocate, uint64_t insn,
                       const InputSection *isec) const = 0;
  virtual void updateSectionContent(InputSection *isec, uint64_t location,
                                    int32_t delta, bool align = false);
  bool getChanged() { return changed; }
  bool getChangedThisIteration() { return changedThisIteration; }
  void resetChanged() { changed = false; }
  void resetChangedThisIteration() { changedThisIteration = false; }
  virtual SmallVector<NewInsnToWrite, 3>
  getTransformInsns(Relocation *reloc,
                    const NanoMipsTransformTemplate *transformTemplate,
                    const NanoMipsInsProperty *insProperty,
                    const NanoMipsRelocProperty *relocProperty,
                    InputSection *isec, uint64_t insn, uint32_t relNum) const;
  std::string getTypeAsString() const;

protected:
  const NanoMipsInsPropertyTable *insPropertyTable;
  // if the code size has been changed during this iteration of this states
  bool changedThisIteration;
  // if the code size has been changed in this state
  bool changed;
  // Used for generating symbols for opposite branches
  static uint32_t newSkipBcSymCount;

private:
  void changeBytes(InputSection *isec, uint64_t location, int32_t count);
};

class NanoMipsTransformExpand : public NanoMipsTransform {
public:
  NanoMipsTransformExpand(const NanoMipsInsPropertyTable *tbl)
      : NanoMipsTransform(tbl) {
    assert(insPropertyTable);
  }
  TransformKind getType() const override { return TransformExpand; }
  const NanoMipsInsProperty *
  getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc,
                 InputSectionBase *isec) const override;
  const NanoMipsTransformTemplate *
  getTransformTemplate(const NanoMipsInsProperty *insProperty, uint32_t relNum,
                       uint64_t valueToRelocate, uint64_t insn,
                       const InputSection *isec) const override;

  SmallVector<NewInsnToWrite, 3> getTransformInsns(
      Relocation *reloc, const NanoMipsTransformTemplate *transformTemplate,
      const NanoMipsInsProperty *insProperty,
      const NanoMipsRelocProperty *relocProperty, InputSection *isec,
      uint64_t insn, uint32_t relNum) const override;

private:
  const NanoMipsTransformTemplate *
  getExpandTransformTemplate(const NanoMipsInsProperty *insProperty,
                             const Relocation &reloc, uint64_t insn,
                             const InputSection *isec) const;
};

class NanoMipsTransformRelax : public NanoMipsTransform {
public:
  NanoMipsTransformRelax(const NanoMipsInsPropertyTable *tbl)
      : NanoMipsTransform(tbl) {
    assert(insPropertyTable);
  }
  TransformKind getType() const override { return TransformRelax; }
  const NanoMipsInsProperty *
  getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc,
                 InputSectionBase *isec) const override;
  const NanoMipsTransformTemplate *
  getTransformTemplate(const NanoMipsInsProperty *insProperty, uint32_t relNum,
                       uint64_t valueToRelocate, uint64_t insn,
                       const InputSection *isec) const override;
};

class NanoMipsTransformNone : public NanoMipsTransform {
public:
  // NanoMipsTransformNone won't use the table so it doesn't need it
  NanoMipsTransformNone(const NanoMipsInsPropertyTable *)
      : NanoMipsTransform(nullptr) {}
  TransformKind getType() const override { return TransformNone; }
  const NanoMipsInsProperty *
  getInsProperty(uint64_t insn, uint64_t insnMask, RelType reloc,
                 InputSectionBase *isec) const override {
    return nullptr;
  }
  const NanoMipsTransformTemplate *
  getTransformTemplate(const NanoMipsInsProperty *insProperty, uint32_t relNum,
                       uint64_t valueToRelocate, uint64_t insn,
                       const InputSection *isec) const override {
    return nullptr;
  }
};

// Driver of transformations
template <class ELFT> class NanoMipsTransformController {
public:
  NanoMipsTransformController()
      : transformRelax(&insPropertyTable), transformExpand(&insPropertyTable),
        transformNone(&insPropertyTable), currentState(&transformNone) {}

  void initState();
  bool relaxOnce(int pass) const;

  // should be called before change state
  bool shouldRunAgain() const { return this->currentState->getChanged(); }

  bool isNone() const { return &transformNone == currentState; }

private:
  // mayRelax refers to any linker transformation (expand or relax)
  bool mayRelax() const {
    // TODO: When the finalize-relocs option is added, change this expression
    // also goes for sort-by-reference option
    return (!config->relocatable && (config->relax || config->expand));
  }

  NanoMipsTransform::TransformKind getType() const {
    return this->currentState->getType();
  }
  bool safeToModify(InputSection *sec) const;
  void initTransformAuxInfo() const;
  void scanAndTransform(InputSection *sec) const;
  void align(InputSection *sec, Relocation &reloc, uint32_t relNum) const;
  void changeState(int pass);

  NanoMipsRelocPropertyTable relocPropertyTable;
  NanoMipsInsPropertyTable insPropertyTable;
  NanoMipsTransformRelax transformRelax;
  NanoMipsTransformExpand transformExpand;
  NanoMipsTransformNone transformNone;
  // This should be declared after transforms
  NanoMipsTransform *currentState;
  // There can be an infinite loop between relax and expand
  // so relaxations are limited to only work up until 15 passes total
  const int relaxPassLimit = 15;
  bool notExpandedYet = true;
};

} // namespace lld::elf

#endif