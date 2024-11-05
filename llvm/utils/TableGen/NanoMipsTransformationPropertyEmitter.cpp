//===- NanoMipsTransformationPropertyEmitter.cpp --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/ArrayRef.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"

using namespace llvm;
// RelocInsPropertyEmitter - This tablegen backend takse an input .td file
// describing relocation and instruction properties and emits macros for their
// generating
// Note: Used in NanoMips only

namespace llvm {

static constexpr const char *RELOC_MACRO_BEGIN = "RELOC_PROPERTY(";
static constexpr const char *COMMA_SPACE = ", ";
static constexpr const char *MACRO_END = ")\n";
static constexpr const char *INS_MACRO_BEGIN = "INS_PROPERTY(";
static constexpr const char *NULLPTR = "nullptr";
static constexpr const char *EXTRACT_REG_MACRO_BEGIN = "EXTRACT_REG(";
static constexpr const char *TRANSFORM_TEMPLATE_MACRO_BEGIN =
    "TRANSFORM_TEMPLATE(";
static constexpr const char *INS_TEMPLATE_MACRO_BEGIN = "INS_TEMPLATE(";
static constexpr const char *BACKSLASH_NEWLINE = "\\\n";
static constexpr const char *INSERT_REG_MACRO_BEGIN = "INSERT_REG(";

void emitRelocStart(raw_ostream &OS) { OS << "#ifdef RELOC_PROPERTY\n\n\n"; }

void emitRelocEnd(raw_ostream &OS) { OS << "\n\n#endif\n\n\n"; }

void emitInsStart(raw_ostream &OS) {
  OS << "#if defined(INS_PROPERTY) && defined(EXTRACT_REG)\n\n\n";
}

void emitInsEnd(raw_ostream &OS) { OS << "\n\n#endif\n\n\n"; }

const Record *verifyAndGetClassOfField(const Record *Record,
                                       std::string FieldName,
                                       std::string ClassName) {
  if (!Record)
    return nullptr;
  const RecordVal *Field = Record->getValue(FieldName);
  if (!Field || !isa<VarInit>(Field->getValue()) ||
      !isa<RecordRecTy>(Field->getType()))
    return nullptr;

  ArrayRef<const llvm::Record *> Classes =
      dyn_cast<RecordRecTy>(Field->getType())->getClasses();
  if (Classes.size() != 1 || Classes[0]->getName() != ClassName)
    return nullptr;
  return Classes[0];
}

bool checkRelocPropertyClass(const Record *RelocPropertyClass) {
  if (!RelocPropertyClass)
    return false;
  const RecordVal *InstSize = RelocPropertyClass->getValue("InstSize");
  if (!InstSize || !isa<IntRecTy>(InstSize->getType()))
    return false;
  const RecordVal *BitsToRelocate =
      RelocPropertyClass->getValue("BitsToRelocate");
  if (!BitsToRelocate || !isa<IntRecTy>(BitsToRelocate->getType()))
    return false;
  const RecordVal *InsMask = RelocPropertyClass->getValue("InsMask");
  if (!InsMask || !isa<BitsRecTy>(InsMask->getType()) ||
      dyn_cast<BitsInit>(InsMask->getValue())->getNumBits() != 64)
    return false;
  return true;
}

void emitRelocProperty(const Record *RelocProperty, raw_ostream &OS) {
  const Init *InsMaskInit = RelocProperty->getValue("InsMask")->getValue();
  OS << RELOC_MACRO_BEGIN << RelocProperty->getName() << COMMA_SPACE
     << RelocProperty->getValue("InstSize")->getValue()->getAsString()
     << COMMA_SPACE
     << RelocProperty->getValue("BitsToRelocate")->getValue()->getAsString()
     << COMMA_SPACE << "0x"
     << utohexstr(
            dyn_cast<IntInit>(InsMaskInit->convertInitializerTo(IntRecTy::get(
                                  InsMaskInit->getRecordKeeper())))
                ->getValue())
     << MACRO_END;
}

bool checkInsIdClass(const Record *InstrIdentifier) {
  if (!InstrIdentifier)
    return false;
  const RecordVal *Name = InstrIdentifier->getValue("Name");
  if (!Name || !isa<StringRecTy>(Name->getType()))
    return false;
  const RecordVal *Opcode = InstrIdentifier->getValue("Opcode");
  if (!Opcode || !isa<BitsRecTy>(Opcode->getType()) ||
      dyn_cast<BitsInit>(Opcode->getValue())->getNumBits() != 64)
    return false;
  return true;
}

bool checkExRegClass(const Record *ExtractReg) {
  if (!ExtractReg)
    return false;
  const RecordVal *Pos = ExtractReg->getValue("Pos");
  if (!Pos || !isa<IntRecTy>(Pos->getType()))
    return false;
  const RecordVal *Size = ExtractReg->getValue("Size");
  if (!Size || !isa<IntRecTy>(Size->getType()))
    return false;
  return true;
}

bool checkInsRegClass(const Record *InsReg) {
  if (!InsReg)
    return false;
  const RecordVal *Pos = InsReg->getValue("Pos");
  if (!Pos || !isa<IntRecTy>(Pos->getType()))
    return false;
  const RecordVal *Size = InsReg->getValue("Size");
  if (!Size || !isa<IntRecTy>(Size->getType()))
    return false;

  if (!verifyAndGetClassOfField(InsReg, "Reg", "RegKind"))
    return false;

  return true;
}

bool checkInsPropertyClass(const Record *InsPropertyClass) {
  if (!InsPropertyClass)
    return false;
  const Record *InstrId = nullptr;
  if (!(InstrId = verifyAndGetClassOfField(InsPropertyClass, "InsId",
                                           "InstructionIdentifier")))
    return false;
  if (!checkInsIdClass(InstrId))
    return false;
  const Record *ExtractReg = nullptr;
  if (!(ExtractReg = verifyAndGetClassOfField(InsPropertyClass, "ExtractTReg",
                                              "ExtractReg")))
    return false;
  if (!checkExRegClass(ExtractReg))
    return false;
  if (!verifyAndGetClassOfField(InsPropertyClass, "ConvertTReg", "ConvertReg"))
    return false;
  if (!verifyAndGetClassOfField(InsPropertyClass, "IsTRegValid", "IsRegValid"))
    return false;
  if (!verifyAndGetClassOfField(InsPropertyClass, "ExtractSReg", "ExtractReg"))
    return false;
  if (!verifyAndGetClassOfField(InsPropertyClass, "ConvertSReg", "ConvertReg"))
    return false;
  if (!verifyAndGetClassOfField(InsPropertyClass, "IsSRegValid", "IsRegValid"))
    return false;
  return true;
}

// Could be changed to write directly to ostream
std::string getInsertRegEmission(const Record *Rec, const std::string Field) {
  std::string RetValue = NULLPTR;
  const DefInit *Init = dyn_cast<DefInit>(Rec->getValue(Field)->getValue());
  if (Init) {
    std::string Tmp;
    raw_string_ostream SS(Tmp);
    SS.reserveExtraSpace(50);
    const IntInit *Pos =
        dyn_cast<IntInit>(Init->getDef()->getValue("Pos")->getValue());
    const IntInit *Size =
        dyn_cast<IntInit>(Init->getDef()->getValue("Size")->getValue());
    const DefInit *Reg =
        dyn_cast<DefInit>(Init->getDef()->getValue("Reg")->getValue());
    if (!Init->getDef()->isAnonymous()) {
      SS << Init->getDef()->getName();
      if (Pos || Size || Reg)
        SS << "<";
      if (Pos)
        SS << Pos->getAsString();
      if (Pos && (Size || Reg))
        SS << COMMA_SPACE;
      if (Size)
        SS << Size->getAsString();
      if (Size && Reg)
        SS << COMMA_SPACE;
      if (Reg)
        SS << Reg->getDef()->getName().str();
      if (Pos || Reg || Size)
        SS << ">";
    } else {
      SS << INSERT_REG_MACRO_BEGIN << Pos->getAsString() << COMMA_SPACE
         << Size->getAsString() << COMMA_SPACE << Reg->getDef()->getName().str()
         << ")";
    }
    RetValue = SS.str();
  }
  return RetValue;
}

std::string getExtractRegEmission(const Record *Rec, const std::string Field) {
  std::string RetValue = NULLPTR;
  const DefInit *Init = dyn_cast<DefInit>(Rec->getValue(Field)->getValue());
  if (Init) {
    std::string Tmp;
    raw_string_ostream SS(Tmp);
    SS.reserveExtraSpace(128);
    const IntInit *Pos =
        dyn_cast<IntInit>(Init->getDef()->getValue("Pos")->getValue());
    const IntInit *Size =
        dyn_cast<IntInit>(Init->getDef()->getValue("Size")->getValue());
    if (!Init->getDef()->isAnonymous()) {
      SS << Init->getDef()->getName().str();
      if (Pos) {
        SS << "<" << Pos->getAsString();
        if (Size)
          SS << COMMA_SPACE << Size->getAsString();
      } else if (Size)
        SS << "<" << Size->getAsString();

      if (Pos || Size)
        SS << ">";
    } else {
      SS << EXTRACT_REG_MACRO_BEGIN << Pos->getAsString() << COMMA_SPACE
         << Size->getAsString() << ")";
    }
    RetValue = SS.str();
  }
  return RetValue;
}

std::string getConvertRegEmission(const Record *Rec, const std::string Field) {
  std::string RetValue = NULLPTR;
  const DefInit *Init = dyn_cast<DefInit>(Rec->getValue(Field)->getValue());
  if (Init)
    RetValue = Init->getDef()->getName().str();
  return RetValue;
}

std::string getIsRegValidEmission(const Record *Rec, const std::string Field) {
  std::string RetValue = NULLPTR;
  const DefInit *Init = dyn_cast<DefInit>(Rec->getValue(Field)->getValue());
  if (Init)
    RetValue = Init->getDef()->getName().str();
  return RetValue;
}

void emitInsProperty(const Record *InsProperty, raw_ostream &OS) {

  const Record *InsId =
      dyn_cast<DefInit>(InsProperty->getValue("InsId")->getValue())->getDef();
  std::string InsName = InsId->getValue("Name")->getValue()->getAsString();
  const Init *OpcodeInit = InsId->getValue("Opcode")->getValue();
  // UnsetInit, for everything unset
  OS << INS_MACRO_BEGIN << InsName << COMMA_SPACE << "0x"
     << utohexstr(
            dyn_cast<IntInit>(OpcodeInit->convertInitializerTo(
                                  IntRecTy::get(OpcodeInit->getRecordKeeper())))
                ->getValue())
     << COMMA_SPACE << getExtractRegEmission(InsProperty, "ExtractTReg")
     << COMMA_SPACE << getConvertRegEmission(InsProperty, "ConvertTReg")
     << COMMA_SPACE << getIsRegValidEmission(InsProperty, "IsTRegValid")
     << COMMA_SPACE << getExtractRegEmission(InsProperty, "ExtractSReg")
     << COMMA_SPACE << getConvertRegEmission(InsProperty, "ConvertSReg")
     << COMMA_SPACE << getIsRegValidEmission(InsProperty, "IsSRegValid")
     << MACRO_END;
}

void emitTransformEnumStart(raw_ostream &OS, const Record *Arch) {
  OS << "#ifdef TRANSFORM_ENUM\n\n\n enum " << Arch->getName()
     << "TransformType{\n";
}

void emitTransformEnumEnd(raw_ostream &OS) { OS << "};\n\n#endif\n\n\n"; }

std::string transformTypeFullName(const Record *TransformType,
                                  const Record *Arch) {
  std::string Tmp;
  raw_string_ostream SS(Tmp);
  SS.reserveExtraSpace(64);
  std::string ArchUpper = Arch->getName().upper();
  StringRef Type = TransformType->getName();
  size_t UnderScoreIndex = Type.find("_");
  SS << Type.substr(0, UnderScoreIndex) << "_" << ArchUpper
     << Type.substr(UnderScoreIndex);
  return SS.str();
}

void emitTransformType(const Record *TransformType, const Record *Arch,
                       raw_ostream &OS) {
  OS << "\t" << transformTypeFullName(TransformType, Arch) << ",\n";
}

void emitTransformTemplateStart(raw_ostream &OS) {
  OS << "#if defined(TRANSFORM_TEMPLATE) && defined(INS_TEMPLATE) && "
        "defined(INSERT_REG)\n\n\n"
     << "#define INS_TEMPLATES(...) { __VA_ARGS__ }\n"
     << "#define RELOCS(...) { __VA_ARGS__ }\n\n\n";
}

void emitTransformTemplateEnd(raw_ostream &OS) { OS << "\n\n#endif\n\n\n"; }

bool checkTransformTemplateClass(const Record *TransformTemplateClass) {
  if (!TransformTemplateClass)
    return false;
  if (!verifyAndGetClassOfField(TransformTemplateClass, "Type",
                                "TransformationType"))
    return false;
  if (!verifyAndGetClassOfField(TransformTemplateClass, "InsProp",
                                "InstructionProperty"))
    return false;
  const RecordVal *RelocList = TransformTemplateClass->getValue("RelocList");
  if (!RelocList || !isa<DagRecTy>(RelocList->getType()))
    return false;
  const RecordVal *InsnList = TransformTemplateClass->getValue("InsnList");
  if (!InsnList || !isa<DagRecTy>(InsnList->getType()))
    return false;
  return true;
}

bool checkInsTemplateClass(const Record *InsTemplateClass) {
  if (!InsTemplateClass)
    return false;
  const Record *InsId = nullptr;
  if (!(InsId = verifyAndGetClassOfField(InsTemplateClass, "InsId",
                                         "InstructionIdentifier")))
    return false;
  if (!checkInsIdClass(InsId))
    return false;
  // Reloc property is checkd before, no need to recheck
  if (!verifyAndGetClassOfField(InsTemplateClass, "Relocation",
                                "RelocProperty"))
    return false;
  const RecordVal *Size = InsTemplateClass->getValue("Size");
  if (!Size || !isa<IntRecTy>(Size->getType()))
    return false;
  const Record *InsertReg = nullptr;
  if (!(InsertReg = verifyAndGetClassOfField(InsTemplateClass, "InsertTReg",
                                             "InsertReg")))
    return false;
  if (!checkInsRegClass(InsertReg))
    return false;
  if (!verifyAndGetClassOfField(InsTemplateClass, "InsertSReg", "InsertReg"))
    return false;
  return true;
}

std::string getRelocListEmission(const Record *Rec, const Record *Arch,
                                 const std::string Field) {
  std::string Tmp;
  raw_string_ostream SS(Tmp);
  SS.reserveExtraSpace(128);
  const DagInit *RelocListInit =
      dyn_cast<DagInit>(Rec->getValue(Field)->getValue());
  if (!RelocListInit)
    return "RELOCS()";
  if (!isa<DefInit>(RelocListInit->getOperator()) ||
      dyn_cast<DefInit>(RelocListInit->getOperator())->getDef()->getName() !=
          "relocs")
    PrintFatalNote("RelocList dag should start with relocs operator\n");
  SS << "RELOCS(";
  for (auto [i, BaseInit] : llvm::enumerate(RelocListInit->getArgs())) {
    if (i != 0)
      SS << COMMA_SPACE;
    const DefInit *ArgInit = dyn_cast<DefInit>(BaseInit);
    if (!ArgInit || !ArgInit->getDef()->isSubClassOf("RelocProperty"))
      PrintFatalNote("RelocList arg should be of type RelocProperty\n");
    SS << "R_" << Arch->getName().upper() << "_"
       << ArgInit->getDef()->getName().str();
  }
  SS << ")";
  return SS.str();
}

std::string getInsTemplateEmission(const Record *Rec, const std::string Field) {
  std::string Tmp;
  raw_string_ostream SS(Tmp);
  SS.reserveExtraSpace(512);
  const DagInit *InsnListInit =
      dyn_cast<DagInit>(Rec->getValue(Field)->getValue());
  if (!InsnListInit)
    return "INS_TEMPLATES()";
  if (!isa<DefInit>(InsnListInit->getOperator()) ||
      dyn_cast<DefInit>(InsnListInit->getOperator())->getDef()->getName() !=
          "insns")
    PrintFatalNote("InsnList dag should start with insns operator");
  SS << BACKSLASH_NEWLINE << "INS_TEMPLATES(" << BACKSLASH_NEWLINE;
  for (auto [i, BaseInit] : llvm::enumerate(InsnListInit->getArgs())) {
    if (i != 0)
      SS << COMMA_SPACE << BACKSLASH_NEWLINE;
    const DefInit *ArgInit = dyn_cast<DefInit>(BaseInit);
    if (!ArgInit || !ArgInit->getDef()->isSubClassOf("InstructionTemplate"))
      PrintFatalNote("InsnList arg should be of type InstructionTemplate");
    std::string Tmp2;
    raw_string_ostream SS2(Tmp2);
    SS2.reserveExtraSpace(128);
    const Record *InsId =
        dyn_cast<DefInit>(ArgInit->getDef()->getValue("InsId")->getValue())
            ->getDef();
    std::string InsName = InsId->getValue("Name")->getValue()->getAsString();
    const Init *OpcodeInit = InsId->getValue("Opcode")->getValue();
    SS2 << "0x"
        << utohexstr(
               dyn_cast<IntInit>(OpcodeInit->convertInitializerTo(IntRecTy::get(
                                     OpcodeInit->getRecordKeeper())))
                   ->getValue());
    std::string Opcode = SS2.str();
    const Record *Relocation =
        dyn_cast<DefInit>(ArgInit->getDef()->getValue("Relocation")->getValue())
            ->getDef();
    std::string RelocName = Relocation->getName().str();

    SS << "\t\t" << INS_TEMPLATE_MACRO_BEGIN << InsName << COMMA_SPACE << Opcode
       << COMMA_SPACE << RelocName << COMMA_SPACE
       << ArgInit->getDef()->getValue("Size")->getValue()->getAsString()
       << COMMA_SPACE << getInsertRegEmission(ArgInit->getDef(), "InsertTReg")
       << COMMA_SPACE << getInsertRegEmission(ArgInit->getDef(), "InsertSReg")
       << ")";
  }
  SS << BACKSLASH_NEWLINE << ")" << BACKSLASH_NEWLINE;
  return SS.str();
}

void emitTransformTemplate(const Record *TransformTemplate, const Record *Arch,
                           raw_ostream &OS) {
  const DefInit *TypeInit =
      dyn_cast<DefInit>(TransformTemplate->getValue("Type")->getValue());
  std::string TypeName = transformTypeFullName(TypeInit->getDef(), Arch);
  const DefInit *InsPropInit =
      dyn_cast<DefInit>(TransformTemplate->getValue("InsProp")->getValue());
  const DefInit *InsIdInit =
      dyn_cast<DefInit>(InsPropInit->getDef()->getValue("InsId")->getValue());
  const Init *OpcodeInit = InsIdInit->getDef()->getValue("Opcode")->getValue();

  OS << TRANSFORM_TEMPLATE_MACRO_BEGIN << TypeName << COMMA_SPACE << "0x"
     << utohexstr(
            dyn_cast<IntInit>(OpcodeInit->convertInitializerTo(
                                  IntRecTy::get(OpcodeInit->getRecordKeeper())))
                ->getValue())
     << COMMA_SPACE
     << getRelocListEmission(TransformTemplate, Arch, "RelocList")
     << COMMA_SPACE << getInsTemplateEmission(TransformTemplate, "InsnList")
     << MACRO_END;
}

void emitNanoMipsTransformationProperties(const RecordKeeper &Records,
                                          raw_ostream &OS) {

  const Record *RelocPropertyClass = Records.getClass("RelocProperty");
  if (RelocPropertyClass && !checkRelocPropertyClass(RelocPropertyClass))
    PrintFatalNote(
        "RelocProperty class doesn't have needed fields, fields are: \n"
        "InstSize: int\n"
        "BitsToRelocate: int\n"
        "InsMask: bits<64>\n");

  emitSourceFileHeader("Reloc And Instruction Properties Definitions", OS);
  if (RelocPropertyClass) {
    emitRelocStart(OS);
    std::vector<const Record *> RelocProperties =
        Records.getAllDerivedDefinitions("RelocProperty");
    for (const auto *RelocProperty : RelocProperties) {
      emitRelocProperty(RelocProperty, OS);
    }
    emitRelocEnd(OS);
  }
  const Record *InsPropertyClass = Records.getClass("InstructionProperty");
  if (InsPropertyClass && !checkInsPropertyClass(InsPropertyClass))
    PrintFatalNote(
        "InstructionProperty class doesn't have needed fields, fields are: \n"
        "InstructionIdentifier(string Name, bits<64> Opcode) InsId\n"
        "ExtractReg(int pos, int size) ExtractTReg\n"
        "ExtractReg ExtractSReg\n"
        "ConvertReg() ConvertTReg\n"
        "ConvertReg ConvertSReg\n"
        "IsRegValid() IsTRegValid\n"
        "IsRegValid IsSRegValid\n");
  if (InsPropertyClass) {
    emitInsStart(OS);

    std::vector<const Record *> InstProperties =
        Records.getAllDerivedDefinitions("InstructionProperty");
    for (const auto *InsProperty : InstProperties) {
      emitInsProperty(InsProperty, OS);
    }
    emitInsEnd(OS);
  }

  std::vector<const Record *> ArchVector = Records.getAllDerivedDefinitions("Arch");
  if (ArchVector.size() != 1)
    PrintWarning("Should specify one architecture to get transform templates "
                 "and types\n");

  const Record *Arch = ArchVector[0];

  const Record *TransformationTypeClass =
      Records.getClass("TransformationType");
  if (TransformationTypeClass && Arch) {
    emitTransformEnumStart(OS, Arch);
    std::vector<const Record *> TransformTypes =
        Records.getAllDerivedDefinitions("TransformationType");
    for (const auto *TransformType : TransformTypes) {
      emitTransformType(TransformType, Arch, OS);
    }
    emitTransformEnumEnd(OS);
  }

  if (TransformationTypeClass && InsPropertyClass && RelocPropertyClass) {
    const Record *TransformTemplateClass =
        Records.getClass("TransformationTemplate");
    if (TransformTemplateClass &&
        !checkTransformTemplateClass(TransformTemplateClass))
      PrintFatalNote(
          "TransformationTemplate doesn't have needed fields, fields are:\n"
          "TransformationType() Type\n"
          "InstructionProperty(its contents are checked before this) InsProp\n"
          "dag(of RelocProperties) RelocList\n"
          "dag(of InstructionTemplates) InsnList\n");

    const Record *InsTemplateClass = Records.getClass("InstructionTemplate");
    if (InsTemplateClass && !checkInsTemplateClass(InsTemplateClass))
      PrintFatalNote(
          "InstructionTemplate doesn't have needed fields, fields are:\n"
          "InstructionIdentifer(checked earlier) InsId\n"
          "RelocProperty(checked earlier) Relocation\n"
          "InsertReg(int Pos, int Size, RegKind() Reg) InsertTReg\n"
          "InsertReg InsertSReg\n");

    if (TransformTemplateClass && InsTemplateClass && Arch) {
      std::vector<const Record *> TransformTemplates =
          Records.getAllDerivedDefinitions("TransformationTemplate");
      emitTransformTemplateStart(OS);
      for (const auto *TransformTemplate : TransformTemplates) {
        emitTransformTemplate(TransformTemplate, Arch, OS);
      }
      emitTransformTemplateEnd(OS);
    }
  }
}
static TableGen::Emitter::Opt X("gen-nanomips-transformation-properties", emitNanoMipsTransformationProperties,
  "(nanoMIPS only) Generate transformation properties "
);
} // namespace llvm