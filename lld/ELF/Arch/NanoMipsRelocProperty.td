
class Arch;
// Should specify how it is going to be fit in the relocation
def NanoMips: Arch;

class RelocProperty<int instSize, int bitsToRelocate, bits<64> insMask>{
    int InstSize = instSize;
    int BitsToRelocate = bitsToRelocate;
    bits<64> InsMask = insMask;
}

class RelocProperty16<int bitsToRelocate, bits<64> insMask> : RelocProperty<16, bitsToRelocate, insMask>;

// HI and LO refer to the higher and lower set bits of the mask

class RelocProperty16HI6<int bitsToRelocate> : RelocProperty16<bitsToRelocate, 0xfc00>;

class RelocProperty32<int bitsToRelocate, bits<64> insMask> : RelocProperty<32, bitsToRelocate, insMask>;

class RelocProperty32NoMask<int bitsToRelocate> : RelocProperty32<bitsToRelocate, 0>;

class RelocProperty48<int bitsToRelocate, bits<64> insMask> : RelocProperty<48, bitsToRelocate, insMask>;

// 48 bit instructions are treated as 16 bit as their opcode and regs are always in the first 16 bit, the rest 32 are for offset
// and/or immediate

class RelocProperty48HI6LO5<int bitsToRelocate> : RelocProperty48<bitsToRelocate, 0xfc1f>;

def NONE : RelocProperty<0, 0, 0>;
def PC4_S1 : RelocProperty16HI6<5>;
def PC14_S1 : RelocProperty32<15, 0xfc00c000>;
def PC21_S1 : RelocProperty32<22, 0xfc000000>;
def PC_I32 : RelocProperty48HI6LO5<32>;
// The next need to be worked on
def PC7_S1: RelocProperty16HI6<8>;
def PC25_S1: RelocProperty32<26, 0xfe000000>;
def I32: RelocProperty48HI6LO5<32>;
def PC_HI20: RelocProperty32NoMask<20>;
def HI20: RelocProperty32NoMask<20>;
def LO12: RelocProperty32<12, 0xfc00f000>;
// Instruction properties and transforms, from here
// reg is SREG or TREG  depending which reg are we copying 
class RegKind;
def SREG: RegKind;
def TREG: RegKind;

class InsertReg<int pos, int size, RegKind reg>
{
    int Pos = pos;
    int Size = size;
    RegKind Reg = reg;
}

class ExtractReg<int pos, int size>
{
    int Pos = pos;
    int Size = size;
}

class ConvertReg;
class IsRegValid;

def convertReg: ConvertReg;
def isRegValid: IsRegValid;

def moveBalcTReg: ExtractReg<?, ?>;
def moveBalcSReg: ExtractReg<?, ?>;

class InstructionIdentifier<string name, bits<64> opcode>
{
    string Name = name;
    bits<64> Opcode = opcode;
}

class InstructionTemplate<InstructionIdentifier insId, RelocProperty relocation, int size, 
            InsertReg insertTReg,
            InsertReg insertSReg>
{
    InstructionIdentifier InsId = insId;
    RelocProperty Relocation = relocation;
    int Size = size;
    InsertReg InsertTReg = insertTReg;
    InsertReg InsertSReg = insertSReg;
}

class InstructionTemplate16<InstructionIdentifier insId, RelocProperty relocation, InsertReg insertTReg, InsertReg insertSReg> 
        : InstructionTemplate<insId, relocation, 16, insertTReg, insertSReg>;

class InstructionTemplate32<InstructionIdentifier insId, RelocProperty relocation, InsertReg insertTReg, InsertReg insertSReg> 
        : InstructionTemplate<insId, relocation, 32, insertTReg, insertSReg>;

class InstructionTemplate48<InstructionIdentifier insId, RelocProperty relocation, InsertReg insertTReg> 
        : InstructionTemplate<insId, relocation, 48, insertTReg, ?>;

class InstructionProperty<InstructionIdentifier insId, 
        ExtractReg extractTReg, ConvertReg convertTReg, IsRegValid isTRegValid, 
        ExtractReg extractSReg, ConvertReg convertSReg, IsRegValid isSRegValid>
{
    InstructionIdentifier InsId = insId;
    ExtractReg ExtractTReg = extractTReg;
    ConvertReg ConvertTReg = convertTReg;
    IsRegValid IsTRegValid = isTRegValid;
    ExtractReg ExtractSReg = extractSReg;
    ConvertReg ConvertSReg = convertSReg;
    IsRegValid IsSRegValid = isSRegValid;
}

class InstructionProperty48<InstructionIdentifier insId, ExtractReg extractTReg>
        : InstructionProperty<insId, extractTReg, ?, ?, ?, ?, ?>;

class InstructionProperty32<InstructionIdentifier insId, 
        ExtractReg extractTReg, IsRegValid isTRegVal, 
        ExtractReg extractSReg, IsRegValid isSRegValid>
        : InstructionProperty<insId, extractTReg,?, isTRegVal, extractSReg, ?, isSRegValid>;

class InstructionProperty16<InstructionIdentifier insId,
        ExtractReg extractTReg, ConvertReg convertTReg,
        ExtractReg extractSReg, ConvertReg convertSReg>
        : InstructionProperty<insId, extractTReg, convertTReg, ?, extractSReg, convertSReg, ?>;

class InstructionProperty16Conv<InstructionIdentifier insId, ExtractReg extractTReg, ExtractReg extractSReg>
        : InstructionProperty16<insId, extractTReg, convertReg, extractSReg, convertReg>;

class InstructionProperty16NoConv<InstructionIdentifier insId, ExtractReg extractTReg, ExtractReg extractSReg>
        : InstructionProperty16<insId, extractTReg, ?, extractSReg, ?>;

class InstructionProperty32Valid<InstructionIdentifier insId, ExtractReg extractTReg, ExtractReg extractSReg>
        : InstructionProperty32<insId, extractTReg, isRegValid, extractSReg, isRegValid>;

class InstructionProperty32NoValid<InstructionIdentifier insId, ExtractReg extractTReg, ExtractReg extractSReg>
        : InstructionProperty32<insId, extractTReg, ?, extractSReg, ?>;

class TransformationType;


def relocs;
def insns;

class TransformationTemplate<TransformationType type, InstructionProperty insProp, dag relocList, dag insnList>
{
        TransformationType Type = type;
        InstructionProperty InsProp = insProp;
        dag RelocList = relocList;
        dag InsnList = insnList;
}


def BEQC16: InstructionIdentifier<"beqc[16]", 0xd800>;
def BNEC16: InstructionIdentifier<"bnec[16]", 0xd800>;
def BXXC16: InstructionIdentifier<"bxxc[16]", 0xd800>;
def LAPC32: InstructionIdentifier<"lapc", 0x04000000>;
def LAPC48: InstructionIdentifier<"lapc[48]", 0x6003>;
def SWPC48: InstructionIdentifier<"swpc[48]", 0x600f>;
def BEQC32: InstructionIdentifier<"beqc", 0x88000000>;
def BNEC32: InstructionIdentifier<"bnec", 0xa8000000>;
def BGEC: InstructionIdentifier<"bgec", 0x88008000>;
def BLTC: InstructionIdentifier<"bltc", 0xa8008000>;
def BGEUC: InstructionIdentifier<"bgeuc", 0x8800c000>;
def BLTUC: InstructionIdentifier<"bltuc", 0xa800c000>;
def MOVE_BALC: InstructionIdentifier<"move.balc", 0x08000000>;
def BEQZC16: InstructionIdentifier<"beqzc[16]", 0x9800>;
def BC32: InstructionIdentifier<"bc", 0x28000000>;
def BNEZC16: InstructionIdentifier<"bnezc[16]", 0xb800>;
def LI48: InstructionIdentifier<"li[48]", 0x6000>;
def LUI: InstructionIdentifier<"lui", 0xe0000000>;
def ORI: InstructionIdentifier<"ori", 0x80000000>;
def ALUIPC: InstructionIdentifier<"aluipc", 0xe0000002>;
def MOVE16: InstructionIdentifier<"move[16]", 0x1000>; 
def BALC32: InstructionIdentifier<"balc", 0x2a000000>;
def SW32: InstructionIdentifier<"sw", 0x84009000>;
// Needed for not generating zero size array warning
def NOINS: InstructionIdentifier<"noins", 0x0>;

class BEQC16_T: InstructionTemplate16<BEQC16, PC4_S1, InsertReg<7,3,TREG>, InsertReg<4,3,SREG>>;
class BNEC16_T: InstructionTemplate16<BNEC16, PC4_S1, InsertReg<7,3,TREG>, InsertReg<4,3,SREG>>;
class LAPC32_T<RegKind reg>: InstructionTemplate32<LAPC32, PC21_S1, InsertReg<21, 5, reg>, ?>;
class LAPC48_T<RegKind reg>: InstructionTemplate48<LAPC48, PC_I32, InsertReg<5, 5, reg>>;
class SWPC48_T: InstructionTemplate48<SWPC48, PC_I32, InsertReg<5, 5, TREG>>;
class BEQC32_T: InstructionTemplate32<BEQC32, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class BNEC32_T: InstructionTemplate32<BNEC32, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class BEQZC16_T: InstructionTemplate16<BEQZC16, PC7_S1, InsertReg<7, 3, TREG>, ?>;
class BC32_T: InstructionTemplate32<BC32, PC25_S1, ?, ?>;
class BNEZC16_T: InstructionTemplate16<BNEZC16, PC7_S1, InsertReg<7,3, TREG>, ?>;
class BLTC_T: InstructionTemplate32<BLTC, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class BGEC_T: InstructionTemplate32<BGEC, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class BLTUC_T: InstructionTemplate32<BLTUC, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class BGEUC_T: InstructionTemplate32<BGEUC, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class LI48_T: InstructionTemplate48<LI48, I32, InsertReg<5, 5, TREG>>;
class LUI_T<RelocProperty reloc, RegKind treg>: InstructionTemplate32<LUI, reloc, InsertReg<21, 5, treg>, ?>;
class ORI_T<RelocProperty reloc, RegKind treg, RegKind sreg>: InstructionTemplate32<ORI, reloc, InsertReg<21, 5, treg>, InsertReg<16, 5, sreg>>;
class ALUIPC_T<RelocProperty reloc, RegKind treg>: InstructionTemplate32<ALUIPC, reloc, InsertReg<21, 5, TREG>, ?>;
class MOVE16_T: InstructionTemplate16<MOVE16, NONE, InsertReg<0, 5, TREG>, InsertReg<5, 5, SREG>>;
class BALC32_T: InstructionTemplate32<BALC32, PC25_S1, ?, ?>;
class SW32_T<RelocProperty reloc>: InstructionTemplate32<SW32, reloc, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
// Needed for not generating zero size array warning for DISCARD transformation
class NOINS_T: InstructionTemplate<NOINS, NONE, 0, ?, ?>;


def BXXC16_P: InstructionProperty16Conv<BXXC16, ExtractReg<7, 3>, ExtractReg<4, 3>>;
def BEQC32_P: InstructionProperty32Valid<BEQC32, ExtractReg<21, 5>, ExtractReg<16, 5>>;
def BNEC32_P: InstructionProperty32Valid<BNEC32, ExtractReg<21, 5>, ExtractReg<16, 5>>;
def BGEC_P: InstructionProperty32NoValid<BGEC, ExtractReg<21, 5>, ExtractReg<16, 5>>;
def BLTC_P: InstructionProperty32NoValid<BLTC, ExtractReg<21, 5>, ExtractReg<16, 5>>;
def BGEUC_P: InstructionProperty32NoValid<BGEUC, ExtractReg<21, 5>, ExtractReg<16, 5>>;
def BLTUC_P: InstructionProperty32NoValid<BLTUC, ExtractReg<21, 5>, ExtractReg<16, 5>>;
// Note: We call the dreg in move balc sreg, so it would be similar to others 
def MOVE_BALC_P: InstructionProperty32NoValid<MOVE_BALC, moveBalcTReg, moveBalcSReg>;
def LAPC32_P: InstructionProperty32NoValid<LAPC32, ExtractReg<21, 5>, ?>;
def LAPC48_P: InstructionProperty48<LAPC48, ExtractReg<5, 5>>;
def SWPC48_P: InstructionProperty48<SWPC48, ExtractReg<5, 5>>;


def TT_NONE: TransformationType;
def TT_BEQC32: TransformationType;
def TT_BNEC32: TransformationType;
def TT_PCREL16: TransformationType;
def TT_PCREL16_ZERO: TransformationType;
def TT_PCREL32_LONG: TransformationType;
def TT_DISCARD: TransformationType;
def TT_ABS_NMF: TransformationType;
def TT_ABS32_LONG: TransformationType;
def TT_PCREL_NMF: TransformationType;
def TT_IMM48_FIX: TransformationType;
def TT_PCREL32: TransformationType;



def : TransformationTemplate<TT_BEQC32, BXXC16_P, (relocs PC4_S1), (insns BEQC32_T<>)>;
def : TransformationTemplate<TT_BNEC32, BXXC16_P, (relocs PC4_S1), (insns BNEC32_T<>)>;
def : TransformationTemplate<TT_PCREL16, BEQC32_P, (relocs PC14_S1), (insns BEQC16_T<>)>;
def : TransformationTemplate<TT_PCREL16_ZERO, BEQC32_P, (relocs PC14_S1), (insns BEQZC16_T<>)>;
def : TransformationTemplate<TT_PCREL32_LONG, BEQC32_P, (relocs PC14_S1), (insns BNEC32_T<>, BC32_T<>)>;
def : TransformationTemplate<TT_PCREL16, BNEC32_P, (relocs PC14_S1), (insns BNEC16_T<>)>;
def : TransformationTemplate<TT_PCREL16_ZERO, BNEC32_P, (relocs PC14_S1), (insns BNEZC16_T<>)>;
def : TransformationTemplate<TT_PCREL32_LONG, BNEC32_P, (relocs PC14_S1), (insns BEQC32_T<>, BC32_T<>)>;
def : TransformationTemplate<TT_PCREL32_LONG, BGEC_P, (relocs PC14_S1), (insns BLTC_T<>, BC32_T<>)>;
def : TransformationTemplate<TT_PCREL32_LONG, BLTC_P, (relocs PC14_S1), (insns BGEC_T<>, BC32_T<>)>;
def : TransformationTemplate<TT_PCREL32_LONG, BGEUC_P, (relocs PC14_S1), (insns BLTUC_T<>, BC32_T<>)>;
def : TransformationTemplate<TT_PCREL32_LONG, BLTUC_P, (relocs PC14_S1), (insns BGEUC_T<>, BC32_T<>)>;
def : TransformationTemplate<TT_DISCARD, LAPC32_P, (relocs PC21_S1), (insns NOINS_T<>)>;
def : TransformationTemplate<TT_ABS_NMF, LAPC32_P, (relocs PC21_S1), (insns LI48_T<>)>;
def : TransformationTemplate<TT_ABS32_LONG, LAPC32_P, (relocs PC21_S1), (insns LUI_T<HI20, TREG>, ORI_T<LO12,TREG, TREG>)>;
def : TransformationTemplate<TT_PCREL_NMF, LAPC32_P, (relocs PC21_S1), (insns LAPC48_T<TREG>)>;
def : TransformationTemplate<TT_PCREL32_LONG, LAPC32_P, (relocs PC21_S1), (insns ALUIPC_T<PC_HI20, TREG>, ORI_T<LO12, TREG, TREG>)>;
def : TransformationTemplate<TT_PCREL32_LONG, MOVE_BALC_P, (relocs PC21_S1), (insns MOVE16_T<>, BALC32_T<>)>;
def : TransformationTemplate<TT_PCREL32, LAPC48_P, (relocs PC_I32), (insns LAPC32_T<TREG>)>;
def : TransformationTemplate<TT_IMM48_FIX, LAPC48_P, (relocs PC_I32), (insns ALUIPC_T<PC_HI20, TREG>, ORI_T<LO12, TREG, TREG>)>;
def : TransformationTemplate<TT_IMM48_FIX, SWPC48_P, (relocs PC_I32), (insns ALUIPC_T<PC_HI20, SREG>, SW32_T<LO12>)>;