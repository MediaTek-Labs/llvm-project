//===- RelocInsPropertyEmitter.cpp - Reloc and Instr Property Generation ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"
#include "llvm/TableGen/Error.h"

using namespace llvm;
// RelocInsPropertyEmitter - This tablegen backend takse an input .td file
// describing relocation and instruction properties and emits macros for their generating
// Note: Used in NanoMips only
// TODO: Maybe reorganize, Template Method could be used as a design pattern
// TODO: See about uninitialized fields
// Checks are not a guarantee that the program won't crash, need to check if some fields are uninitialized
namespace llvm {

    static constexpr const char *RELOC_MACRO_BEGIN = "RELOC_PROPERTY(";
    static constexpr const char *COMMA_SPACE = ", ";
    static constexpr const char *MACRO_END = ")\n";
    static constexpr const char *INS_MACRO_BEGIN = "INS_PROPERTY(";
    static constexpr const char *NULLPTR = "nullptr";
    static constexpr const char *EXTRACT_REG_MACRO_BEGIN = "EXTRACT_REG(";
    static constexpr const char *TRANSFORM_TEMPLATE_MACRO_BEGIN = "TRANSFORM_TEMPLATE(";
    static constexpr const char *INS_TEMPLATE_MACRO_BEGIN = "INS_TEMPLATE(";
    static constexpr const char *BACKSLASH_NEWLINE = "\\\n";
    static constexpr const char *INSERT_REG_MACRO_BEGIN = "INSERT_REG(";

    void emitRelocStart(raw_ostream &OS)
    {
        OS << "#ifdef RELOC_PROPERTY\n\n\n";
    }

    void emitRelocEnd(raw_ostream &OS)
    {
        OS << "\n\n#endif\n\n\n";
    }

    void emitInsStart(raw_ostream &OS)
    {
        OS << "#if defined(INS_PROPERTY) && defined(EXTRACT_REG)\n\n\n";
    }

    void emitInsEnd(raw_ostream &OS)
    {
        OS << "\n\n#endif\n\n\n";
    }

    // Checking is not that important for now
    const Record *verifyAndGetClassOfField(const Record *record, std::string fieldName, std::string className)
    {
        if(!record) return nullptr;
        const RecordVal *field = record->getValue(fieldName);
        if(!field || !isa<VarInit>(field->getValue()) || !isa<RecordRecTy>(field->getType())) return nullptr;
        auto classes = dyn_cast<RecordRecTy>(field->getType())->getClasses();
        if(classes.size() != 1 || classes[0]->getName() != className) return nullptr;
        return classes[0];
    }
   
    bool checkRelocPropertyClass(const Record *relocPropertyClass)
    {
        if(!relocPropertyClass) return false;
        const RecordVal *instSize = relocPropertyClass->getValue("InstSize");
        if(!instSize || !isa<IntRecTy>(instSize->getType())) return false;
        const RecordVal *bitsToRelocate = relocPropertyClass->getValue("BitsToRelocate");
        if(!bitsToRelocate || !isa<IntRecTy>(bitsToRelocate->getType())) return false;
        const RecordVal *insMask = relocPropertyClass->getValue("InsMask");
        if(!insMask || !isa<BitsRecTy>(insMask->getType()) || dyn_cast<BitsInit>(insMask->getValue())->getNumBits() != 64) return false;
        return true;
    }

    void emitRelocProperty(const Record *relocProperty, raw_ostream &OS)
    {

        OS << RELOC_MACRO_BEGIN
        << relocProperty->getName()
        << COMMA_SPACE
        << relocProperty->getValue("InstSize")->getValue()->getAsString()
        << COMMA_SPACE
        << relocProperty->getValue("BitsToRelocate")->getValue()->getAsString()
        << COMMA_SPACE
        << "0x" << utohexstr(dyn_cast<IntInit>(relocProperty->getValue("InsMask")->getValue()->convertInitializerTo(IntRecTy::get()))->getValue())
        << MACRO_END;

    }

    bool checkInsIdClass(const Record *instrIdentifier)
    {
        if(!instrIdentifier) return false;
        const RecordVal *name = instrIdentifier->getValue("Name");
        if(!name || !isa<StringRecTy>(name->getType())) return false;
        const RecordVal *opcode = instrIdentifier->getValue("Opcode");  
        if(!opcode || !isa<BitsRecTy>(opcode->getType()) || dyn_cast<BitsInit>(opcode->getValue())->getNumBits() != 64) return false;
        return true;
    }

    bool checkExRegClass(const Record *extractReg)
    {
        if(!extractReg) return false;
        const RecordVal *pos = extractReg->getValue("Pos");
        if(!pos || !isa<IntRecTy>(pos->getType())) return false;
        const RecordVal *size = extractReg->getValue("Size");
        if(!size || !isa<IntRecTy>(size->getType())) return false;
        return true;
    }

    bool checkInsRegClass(const Record *insReg)
    {
        if(!insReg) return false;
        const RecordVal *pos = insReg->getValue("Pos");
        if(!pos || !isa<IntRecTy>(pos->getType())) return false;
        const RecordVal *size = insReg->getValue("Size");
        if(!size || !isa<IntRecTy>(size->getType())) return false;

        if(!verifyAndGetClassOfField(insReg, "Reg", "RegKind")) return false;

        return true;
    }

    bool checkInsPropertyClass(const Record *insPropertyClass)
    {
        if(!insPropertyClass) return false;
        const Record *instrId = nullptr;
        if(!(instrId = verifyAndGetClassOfField(insPropertyClass, "InsId", "InstructionIdentifier"))) return false;
        if(!checkInsIdClass(instrId)) return false;
        const Record *extractReg = nullptr;
        if(!(extractReg = verifyAndGetClassOfField(insPropertyClass, "ExtractTReg", "ExtractReg"))) return false;
        if(!checkExRegClass(extractReg)) return false;
        if(!verifyAndGetClassOfField(insPropertyClass, "ConvertTReg", "ConvertReg")) return false;
        if(!verifyAndGetClassOfField(insPropertyClass, "IsTRegValid", "IsRegValid")) return false;
        if(!verifyAndGetClassOfField(insPropertyClass, "ExtractSReg", "ExtractReg")) return false;
        if(!verifyAndGetClassOfField(insPropertyClass, "ConvertSReg", "ConvertReg")) return false;
        if(!verifyAndGetClassOfField(insPropertyClass, "IsSRegValid", "IsRegValid")) return false;
        return true;
    }   

    // Could be changed to write directly to ostream
    std::string getInsertRegEmission(const Record *rec, const std::string field)
    {
        std::string retValue = NULLPTR;
        const DefInit *init = dyn_cast<DefInit>(rec->getValue(field)->getValue());
        if(init)
        {
            std::string tmp;
            raw_string_ostream SS(tmp);
            SS.reserveExtraSpace(50);
            const IntInit *pos = dyn_cast<IntInit>(init->getDef()->getValue("Pos")->getValue());
            const IntInit *size = dyn_cast<IntInit>(init->getDef()->getValue("Size")->getValue());
            const DefInit *reg = dyn_cast<DefInit>(init->getDef()->getValue("Reg")->getValue());
            if(!init->getDef()->isAnonymous())
            {
                SS << init->getDef()->getName();
                if(pos || size || reg) SS << "<";
                if(pos) SS << pos->getAsString();
                if(pos && (size || reg)) SS << COMMA_SPACE;
                if(size) SS << size->getAsString();
                if(size && reg) SS << COMMA_SPACE;
                if(reg) SS << reg->getDef()->getName().str();
                if(pos || reg || size) SS << ">";
            }
            else{
                SS << INSERT_REG_MACRO_BEGIN
                << pos->getAsString()
                << COMMA_SPACE
                << size->getAsString()
                << COMMA_SPACE
                << reg->getDef()->getName().str()
                << ")";
            }
            retValue = SS.str();
        }
        return retValue;
    }

    std::string getExtractRegEmission(const Record *rec, const std::string field)
    {   
        std::string retValue = NULLPTR;
        const DefInit *init = dyn_cast<DefInit>(rec->getValue(field)->getValue());
        if(init)
        {
            std::string tmp;
            raw_string_ostream SS(tmp);
            SS.reserveExtraSpace(128);
            const IntInit *pos = dyn_cast<IntInit>(init->getDef()->getValue("Pos")->getValue());
            const IntInit *size = dyn_cast<IntInit>(init->getDef()->getValue("Size")->getValue());
            if(!init->getDef()->isAnonymous())
            {
                SS << init->getDef()->getName().str();
                if(pos)
                { 
                    SS << "<" << pos->getAsString();
                    if(size) SS << COMMA_SPACE << size->getAsString();  
                }
                else if(size) SS << "<" << size->getAsString();

                if(pos || size) SS << ">";
            }
            else{
                SS << EXTRACT_REG_MACRO_BEGIN
                << pos->getAsString()
                << COMMA_SPACE
                << size->getAsString()
                << ")";
            }
            retValue = SS.str();
        }
        return retValue;
    }

    std::string getConvertRegEmission(const Record *rec, const std::string field)
    {
        std::string retValue = NULLPTR;
        const DefInit *init = dyn_cast<DefInit>(rec->getValue(field)->getValue());
        if(init) retValue = init->getDef()->getName().str();
        return retValue;
    }

    std::string getIsRegValidEmission(const Record *rec, const std::string field)
    {
        std::string retValue = NULLPTR;
        const DefInit *init = dyn_cast<DefInit>(rec->getValue(field)->getValue());
        if(init) retValue = init->getDef()->getName().str();
        return retValue;
    }

    void emitInsProperty(const Record *insProperty, raw_ostream &OS)
    {

        Record *insId = dyn_cast<DefInit>(insProperty->getValue("InsId")->getValue())->getDef();
        std::string insName = insId->getValue("Name")->getValue()->getAsString();
        // UnsetInit, for everything unset
        OS << INS_MACRO_BEGIN
        << insName
        << COMMA_SPACE
        << "0x" << utohexstr(dyn_cast<IntInit>(insId->getValue("Opcode")->getValue()->convertInitializerTo(IntRecTy::get()))->getValue())
        << COMMA_SPACE
        << getExtractRegEmission(insProperty, "ExtractTReg")
        << COMMA_SPACE
        << getConvertRegEmission(insProperty, "ConvertTReg")
        << COMMA_SPACE
        << getIsRegValidEmission(insProperty, "IsTRegValid")
        << COMMA_SPACE
        << getExtractRegEmission(insProperty, "ExtractSReg")
        << COMMA_SPACE
        << getConvertRegEmission(insProperty, "ConvertSReg")
        << COMMA_SPACE
        << getIsRegValidEmission(insProperty, "IsSRegValid")
        << MACRO_END;
    }

    void emitTransformEnumStart(raw_ostream &OS, const Record *arch)
    {
        OS << "#ifdef TRANSFORM_ENUM\n\n\n enum "
        << arch->getName()
        << "TransformType{\n";
    }
    
    void emitTransformEnumEnd(raw_ostream &OS)
    {
        OS << "};\n\n#endif\n\n\n";
    }

    std::string transformTypeFullName(const Record *transformType, const Record *arch)
    {
        std::string tmp;
        raw_string_ostream SS(tmp);
        SS.reserveExtraSpace(64);
        std::string archUpper = arch->getName().upper();
        StringRef type = transformType->getName();
        size_t underScoreIndex = type.find("_");
        SS << type.substr(0, underScoreIndex)
        << "_"
        << archUpper
        << type.substr(underScoreIndex);
        return SS.str();
    }

    void emitTransformType(const Record *transformType, const Record *arch, raw_ostream &OS)
    {
        OS << "\t"
        << transformTypeFullName(transformType, arch)
        << ",\n";
    }

    void emitTransformTemplateStart(raw_ostream &OS)
    {
        OS << "#if defined(TRANSFORM_TEMPLATE) && defined(INS_TEMPLATE) && defined(INSERT_REG)\n\n\n"
        << "#define INS_TEMPLATES(...) { __VA_ARGS__ }\n" 
        << "#define RELOCS(...) { __VA_ARGS__ }\n\n\n";
    }

    void emitTransformTemplateEnd(raw_ostream &OS)
    {
        OS << "\n\n#endif\n\n\n";
    }

    bool checkTransformTemplateClass(const Record *transformTemplateClass)
    {
        if(!transformTemplateClass) return false;
        if(!verifyAndGetClassOfField(transformTemplateClass, "Type", "TransformationType")) return false;
        if(!verifyAndGetClassOfField(transformTemplateClass, "InsProp", "InstructionProperty")) return false;
        const RecordVal *relocList = transformTemplateClass->getValue("RelocList");
        if(!relocList || !isa<DagRecTy>(relocList->getType())) return false;
        const RecordVal *insnList = transformTemplateClass->getValue("InsnList");
        if(!insnList || !isa<DagRecTy>(insnList->getType())) return false;
        return true;
    }

    bool checkInsTemplateClass(const Record *insTemplateClass)
    {
        if(!insTemplateClass) return false;
        const Record *insId = nullptr;
        if(!(insId = verifyAndGetClassOfField(insTemplateClass, "InsId", "InstructionIdentifier"))) return false;
        if(!checkInsIdClass(insId)) return false;
        // Reloc property is checkd before, no need to recheck
        if(!verifyAndGetClassOfField(insTemplateClass, "Relocation", "RelocProperty")) return false;
        const RecordVal *size = insTemplateClass->getValue("Size");
        if(!size || !isa<IntRecTy>(size->getType())) return false;
        const Record *insertReg = nullptr;
        if(!(insertReg = verifyAndGetClassOfField(insTemplateClass, "InsertTReg", "InsertReg"))) return false;
        if(!checkInsRegClass(insertReg)) return false;
        if(!verifyAndGetClassOfField(insTemplateClass, "InsertSReg", "InsertReg")) return false;
        return true;
    }

    std::string getRelocListEmission(const Record *rec, const Record *arch, const std::string field)
    {
        std::string tmp;
        raw_string_ostream SS(tmp);
        SS.reserveExtraSpace(128);
        const DagInit *relocListInit = dyn_cast<DagInit>(rec->getValue(field)->getValue());
        if(!relocListInit) return "RELOCS()";
        if(!isa<DefInit>(relocListInit->getOperator()) || 
            dyn_cast<DefInit>(relocListInit->getOperator())->getDef()->getName() != "relocs")
            PrintFatalNote("RelocList dag should start with relocs operator\n"); 
        SS << "RELOCS(";
        for(auto it = relocListInit->arg_begin(); it != relocListInit->arg_end(); it++)
        {
            if(it != relocListInit->arg_begin()) SS << COMMA_SPACE;
            const DefInit *argInit = dyn_cast<DefInit>(*it);
            if(!argInit || !argInit->getDef()->isSubClassOf("RelocProperty"))
                PrintFatalNote("RelocList arg should be of type RelocProperty\n");
            SS << "R_" << arch->getName().upper() << "_" << dyn_cast<DefInit>(*it)->getDef()->getName().str();
        }
        SS << ")";
        return SS.str();
    }

    std::string getInsTemplateEmission(const Record *rec, const std::string field)
    {
        std::string tmp;
        raw_string_ostream SS(tmp);
        SS.reserveExtraSpace(512);
        const DagInit *insnListInit = dyn_cast<DagInit>(rec->getValue(field)->getValue());
        if(!insnListInit) return "INS_TEMPLATES()";
        if(!isa<DefInit>(insnListInit->getOperator()) ||
            dyn_cast<DefInit>(insnListInit->getOperator())->getDef()->getName() != "insns")
            PrintFatalNote("InsnList dag should start with insns operator");
        SS << BACKSLASH_NEWLINE << "INS_TEMPLATES(" << BACKSLASH_NEWLINE;
        for(auto it = insnListInit->arg_begin(); it != insnListInit->arg_end(); it++)
        {
            if(it != insnListInit->arg_begin()) SS << COMMA_SPACE << BACKSLASH_NEWLINE;
            const DefInit *argInit = dyn_cast<DefInit>(*it);
            if(!argInit || !argInit->getDef()->isSubClassOf("InstructionTemplate"))
                PrintFatalNote("InsnList arg should be og type InstructionTemplate");
            std::string tmp2;
            raw_string_ostream SS2(tmp2);
            SS2.reserveExtraSpace(128);
            const Record *insId = dyn_cast<DefInit>(argInit->getDef()->getValue("InsId")->getValue())->getDef();
            std::string insName = insId->getValue("Name")->getValue()->getAsString();
            SS2 << "0x" << utohexstr(dyn_cast<IntInit>(insId->getValue("Opcode")->getValue()->convertInitializerTo(IntRecTy::get()))->getValue());
            std::string opcode = SS2.str();
            const Record *relocation = dyn_cast<DefInit>(argInit->getDef()->getValue("Relocation")->getValue())->getDef();
            std::string relocName = relocation->getName().str();
            

            SS << "\t\t"
               << INS_TEMPLATE_MACRO_BEGIN
               << insName
               << COMMA_SPACE
               << opcode
               << COMMA_SPACE
               << relocName
               << COMMA_SPACE
               << argInit->getDef()->getValue("Size")->getValue()->getAsString()
               << COMMA_SPACE
               << getInsertRegEmission(argInit->getDef(), "InsertTReg")
               << COMMA_SPACE
               << getInsertRegEmission(argInit->getDef(), "InsertSReg")
               << ")";
            
        }
        SS << BACKSLASH_NEWLINE << ")" << BACKSLASH_NEWLINE;
        return SS.str();
    }

    void emitTransformTemplate(const Record *transformTemplate, const Record *arch, raw_ostream &OS)
    {
        const DefInit *typeInit = dyn_cast<DefInit>(transformTemplate->getValue("Type")->getValue());
        std::string typeName = transformTypeFullName(typeInit->getDef(), arch);
        const DefInit *insPropInit = dyn_cast<DefInit>(transformTemplate->getValue("InsProp")->getValue());
        const DefInit *insIdInit = dyn_cast<DefInit>(insPropInit->getDef()->getValue("InsId")->getValue());
        OS << TRANSFORM_TEMPLATE_MACRO_BEGIN
        << typeName
        << COMMA_SPACE
        << "0x" << utohexstr(dyn_cast<IntInit>(insIdInit->getDef()->getValue("Opcode")->getValue()->convertInitializerTo(IntRecTy::get()))->getValue())
        << COMMA_SPACE
        << getRelocListEmission(transformTemplate, arch, "RelocList")
        << COMMA_SPACE
        << getInsTemplateEmission(transformTemplate, "InsnList")
        << MACRO_END;
    }

    void EmitRelocInsProperties(RecordKeeper &Records, raw_ostream &OS) {

        const Record *relocPropertyClass = Records.getClass("RelocProperty");
        if(relocPropertyClass && !checkRelocPropertyClass(relocPropertyClass)) 
            PrintFatalNote("RelocProperty class doesn't have needed fields, fields are: \n"
                           "InstSize: int\n"
                           "BitsToRelocate: int\n"
                           "InsMask: bits<64>\n");

        emitSourceFileHeader("Reloc And Instruction Properties Definitions", OS);
        if(relocPropertyClass)
        {
            emitRelocStart(OS);
            std::vector<Record *> RelocProperties = Records.getAllDerivedDefinitions("RelocProperty");
            for(const auto *relocProperty : RelocProperties)
            {
                    emitRelocProperty(relocProperty, OS);
            }
            emitRelocEnd(OS);
        }
        const Record *insPropertyClass = Records.getClass("InstructionProperty");
        if(insPropertyClass && !checkInsPropertyClass(insPropertyClass))
            PrintFatalNote("InstructionProperty class doesn't have needed fields, fields are: \n"
            "InstructionIdentifier(string Name, bits<64> Opcode) InsId\n"
            "ExtractReg(int pos, int size) ExtractTReg\n"
            "ExtractReg ExtractSReg\n"
            "ConvertReg() ConvertTReg\n"
            "ConvertReg ConvertSReg\n"
            "IsRegValid() IsTRegValid\n"
            "IsRegValid IsSRegValid\n");
        if(insPropertyClass)
        {
            emitInsStart(OS);

            std::vector<Record *> InstProperties = Records.getAllDerivedDefinitions("InstructionProperty");
            for(const auto *insProperty : InstProperties)
            {
                emitInsProperty(insProperty, OS);
            }
            emitInsEnd(OS);
        }

        std::vector<Record *> archVector = Records.getAllDerivedDefinitions("Arch");
        if(archVector.size() != 1)
            PrintWarning("Should specify one architecture to get transform templates and types\n");
            
        const Record *arch = archVector[0];
        
        const Record *transformationTypeClass = Records.getClass("TransformationType");
        if(transformationTypeClass && arch)
        {
            emitTransformEnumStart(OS, arch);
            std::vector<Record *> TransformTypes = Records.getAllDerivedDefinitions("TransformationType");
            for(const auto *transformType: TransformTypes)
            {
                emitTransformType(transformType, arch, OS);
            }
            emitTransformEnumEnd(OS);
        }

        if(transformationTypeClass && insPropertyClass && relocPropertyClass)
        {
            const Record *transformTemplateClass = Records.getClass("TransformationTemplate");
            if(transformTemplateClass && !checkTransformTemplateClass(transformTemplateClass))
                PrintFatalNote("TransformationTemplate doesn't have needed fields, fields are:\n"
                               "TransformationType() Type\n"
                               "InstructionProperty(its contents are checked before this) InsProp\n"
                               "dag(of RelocProperties) RelocList\n"
                               "dag(of InstructionTemplates) InsnList\n"
                );

            const Record *insTemplateClass = Records.getClass("InstructionTemplate");
            if(insTemplateClass && !checkInsTemplateClass(insTemplateClass))
                PrintFatalNote("InstructionTemplate doesn't have needed fields, fields are:\n"
                               "InstructionIdentifer(checked earlier) InsId\n"
                               "RelocProperty(checked earlier) Relocation\n"
                               "InsertReg(int Pos, int Size, RegKind() Reg) InsertTReg\n"
                               "InsertReg InsertSReg\n"
                );

            if(transformTemplateClass && insTemplateClass && arch)
            {
                std::vector<Record *> transformTemplates = Records.getAllDerivedDefinitions("TransformationTemplate");
                emitTransformTemplateStart(OS);
                for(const auto *transformTemplate: transformTemplates)
                {
                    emitTransformTemplate(transformTemplate, arch, OS);
                }
                emitTransformTemplateEnd(OS);
            }
        }
    }
}