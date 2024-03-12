
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

// ------------------------------- RelocProperty -------------------------------

def NONE : RelocProperty<0, 0, 0>;
def ALIGN : RelocProperty<0, 0, 0>;
def PC4_S1 : RelocProperty16HI6<5>;
def PC14_S1 : RelocProperty32<15, 0xfc00c000>;
// PC21_S1 discard for gpsetup remaining
def PC21_S1 : RelocProperty32<22, 0xfc000000>;
def PC_I32 : RelocProperty48HI6LO5<32>;
def GPREL19_S2: RelocProperty32<21, 0xfc000003>;
def GPREL18: RelocProperty32<18, 0xfc1c0000>;
def GPREL17_S1: RelocProperty32<18, 0xfc1c0001>;
def PC10_S1: RelocProperty16HI6<11>;
def PC7_S1: RelocProperty16HI6<8>;
// Currently doing
def SAVERESTORE: RelocProperty32<32, 0xfc10f007>;
// The next need to be worked on
def PC25_S1: RelocProperty32<26, 0xfe000000>;
def I32: RelocProperty48HI6LO5<32>;
def PC_HI20: RelocProperty32NoMask<20>;
def HI20: RelocProperty32NoMask<20>;
def LO12: RelocProperty32<12, 0xfc00f000>;
def GPREL7_S2: RelocProperty16HI6<9>;
def GPREL_I32: RelocProperty48HI6LO5<32>;
def GPREL_HI20: RelocProperty32NoMask<20>;
def GPREL_LO12: RelocProperty32NoMask<12>;

// ----------------------------- End RelocProperty -----------------------------


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
def isStoreSrcRegValid: IsRegValid;

def moveBalcTReg: ExtractReg<?, ?>;
def moveBalcSReg: ExtractReg<?, ?>;
def extractSaveResFields: ExtractReg<?, ?>;

def insertSaveResFields: InsertReg<?, ? ,?>;
def insertSaveRes16Fields: InsertReg<?, ?, ?>;

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
        : InstructionProperty<insId, extractTReg, ?, isTRegVal, extractSReg, ?, isSRegValid>;

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

// --------------------------- InstructionIdentifier ---------------------------

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

def ADDIUGPW32: InstructionIdentifier<"addiu[gp.w]", 0x40000000>;
def LWGP32: InstructionIdentifier<"lw[gp]", 0x40000002>;
def SWGP32: InstructionIdentifier<"sw[gp]", 0x40000003>;
def LW32: InstructionIdentifier<"lw", 0x84008000>;
def LWPC48: InstructionIdentifier<"lwpc[48]", 0x600b>;
def LWGP16: InstructionIdentifier<"lw[gp16]", 0x5400>;
def ADDIUGP48: InstructionIdentifier<"addiugp[48]", 0x6002>;
// There is no addugp instruction it is addu, it is called addugp
// as one of the operands will be gp
def ADDUGP: InstructionIdentifier<"addu", 0x23800150>;
def SWGP16: InstructionIdentifier<"sw[gp16]", 0xd400>;

def ADDIUGPB32: InstructionIdentifier<"addiu[gp.b]", 0x440c0000>;
def LBGP32: InstructionIdentifier<"lb[gp]", 0x44000000>;
def LBUGP32: InstructionIdentifier<"lbu[gp]", 0x44080000>;
def SBGP32: InstructionIdentifier<"sb[gp]", 0x44040000>;
def LB32: InstructionIdentifier<"lb", 0x84000000>;
def LBU32: InstructionIdentifier<"lbu", 0x84002000>;
def SB32: InstructionIdentifier<"sb", 0x84001000>;
def LHGP32: InstructionIdentifier<"lh[gp]", 0x44100000>;
def LHUGP32: InstructionIdentifier<"lhu[gp]", 0x44100001>;
def SHGP32: InstructionIdentifier<"sh[gp]", 0x44140000>;
def LH32: InstructionIdentifier<"lh", 0x84004000>;
def LHU32: InstructionIdentifier<"lhu", 0x84006000>;
def SH32: InstructionIdentifier<"lhu", 0x84005000>;

def BALC16: InstructionIdentifier<"balc[16]", 0x3800>;
def BC16: InstructionIdentifier<"bc[16]", 0x1800>;

def RESTOREGP: InstructionIdentifier<"restore[gp]", 0x80003006>;
def RESTOREJRCGP: InstructionIdentifier<"restore.jrc[gp]", 0x80003007>;
def RESTORE32: InstructionIdentifier<"restore", 0x80003002>;
def RESTOREJRC32: InstructionIdentifier<"restore.jrc", 0x80003003>;
def RESTOREJRC16: InstructionIdentifier<"restore.jrc[16]", 0x1d00>;
def SAVEGP: InstructionIdentifier<"save[gp]", 0x80003004>;
def SAVE32: InstructionIdentifier<"save", 0x80003000>;
def SAVE16: InstructionIdentifier<"save[16]", 0x1c00>;

// ------------------------- End InstructionIdentifier -------------------------

// ---------------------------- InstructionTemplate ----------------------------

class BEQC16_T: InstructionTemplate16<BEQC16, PC4_S1, InsertReg<7,3,TREG>, InsertReg<4,3,SREG>>;
class BNEC16_T: InstructionTemplate16<BNEC16, PC4_S1, InsertReg<7,3,TREG>, InsertReg<4,3,SREG>>;

class LAPC32_T<RegKind treg>: InstructionTemplate32<LAPC32, PC21_S1, InsertReg<21, 5, treg>, ?>;

class LAPC48_T<RegKind treg>: InstructionTemplate48<LAPC48, PC_I32, InsertReg<5, 5, treg>>;
class SWPC48_T: InstructionTemplate48<SWPC48, PC_I32, InsertReg<5, 5, TREG>>;

class BEQC32_T: InstructionTemplate32<BEQC32, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class BNEC32_T: InstructionTemplate32<BNEC32, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;

class BEQZC16_T: InstructionTemplate16<BEQZC16, PC7_S1, InsertReg<7, 3, TREG>, ?>;
class BNEZC16_T: InstructionTemplate16<BNEZC16, PC7_S1, InsertReg<7,3, TREG>, ?>;

class BC32_T: InstructionTemplate32<BC32, PC25_S1, ?, ?>;
class BALC32_T: InstructionTemplate32<BALC32, PC25_S1, ?, ?>;

class BLTC_T: InstructionTemplate32<BLTC, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class BGEC_T: InstructionTemplate32<BGEC, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class BLTUC_T: InstructionTemplate32<BLTUC, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class BGEUC_T: InstructionTemplate32<BGEUC, PC14_S1, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;

class LI48_T: InstructionTemplate48<LI48, I32, InsertReg<5, 5, TREG>>;

class LUI_T<RelocProperty reloc, RegKind treg>: InstructionTemplate32<LUI, reloc, InsertReg<21, 5, treg>, ?>;
class ORI_T<RelocProperty reloc, RegKind treg, RegKind sreg>: InstructionTemplate32<ORI, reloc, InsertReg<21, 5, treg>, InsertReg<16, 5, sreg>>;
class ALUIPC_T<RelocProperty reloc, RegKind treg>: InstructionTemplate32<ALUIPC, reloc, InsertReg<21, 5, treg>, ?>;
class ADDIUGP48_T<RelocProperty reloc, RegKind treg>: InstructionTemplate48<ADDIUGP48, reloc, InsertReg<5, 5, treg>>;

class MOVE16_T: InstructionTemplate16<MOVE16, NONE, InsertReg<0, 5, TREG>, InsertReg<5, 5, SREG>>;
// Needed for not generating zero size array warning for DISCARD transformation
class NOINS_T: InstructionTemplate<NOINS, NONE, 0, ?, ?>;

class SW32_T<RelocProperty reloc>: InstructionTemplate32<SW32, reloc, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class LW32_T<RelocProperty reloc>: InstructionTemplate32<LW32, reloc, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class LWPC48_T<RelocProperty reloc>: InstructionTemplate48<LWPC48, reloc, InsertReg<5, 5, TREG>>;
class LB32_T<RelocProperty reloc>: InstructionTemplate32<LB32, reloc, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class LBU32_T<RelocProperty reloc>: InstructionTemplate32<LBU32, reloc, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class SB32_T<RelocProperty reloc>: InstructionTemplate32<SB32, reloc, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class LH32_T<RelocProperty reloc>: InstructionTemplate32<LH32, reloc, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class LHU32_T<RelocProperty reloc>: InstructionTemplate32<LHU32, reloc, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;
class SH32_T<RelocProperty reloc>: InstructionTemplate32<SH32, reloc, InsertReg<21, 5, TREG>, InsertReg<16, 5, SREG>>;

class ADDIUGPW32_T: InstructionTemplate32<ADDIUGPW32, GPREL19_S2, InsertReg<21, 5, TREG>, ?>;
class LWGP32_T: InstructionTemplate32<LWGP32, GPREL19_S2, InsertReg<21, 5, TREG>, ?>;
class SWGP32_T: InstructionTemplate32<SWGP32, GPREL19_S2, InsertReg<21, 5, TREG>, ?>;

class LWGP16_T: InstructionTemplate16<LWGP16, GPREL7_S2, InsertReg<7, 3, TREG>, ?>;
class SWGP16_T: InstructionTemplate16<SWGP16, GPREL7_S2, InsertReg<7, 3, TREG>, ?>;

class ADDUGP_T<RegKind treg, RegKind sreg>: InstructionTemplate32<ADDUGP, NONE, InsertReg<16, 5, treg>, InsertReg<11, 5, sreg>>;
// Here, treg is actually count
class RESTORE32_T: InstructionTemplate32<RESTORE32, NONE, InsertReg<16, 4, TREG>, insertSaveResFields>;
class RESTOREJRC32_T: InstructionTemplate32<RESTOREJRC32, NONE, InsertReg<16, 4, TREG>, insertSaveResFields>;
class RESTOREJRC16_T: InstructionTemplate16<RESTOREJRC16, NONE, InsertReg<0, 4, TREG>, insertSaveRes16Fields>;
class SAVE32_T: InstructionTemplate32<SAVE32, NONE, InsertReg<16, 4, TREG>, insertSaveResFields>;
class SAVE16_T: InstructionTemplate16<SAVE16, NONE, InsertReg<0, 4, TREG>, insertSaveRes16Fields>;

class ADDIUGPB32_T: InstructionTemplate32<ADDIUGPB32, GPREL18, InsertReg<21, 5, TREG>, ?>;
class LBGP32_T: InstructionTemplate32<LBGP32, GPREL18, InsertReg<21, 5, TREG>, ?>;
class LBUGP32_T: InstructionTemplate32<LBUGP32, GPREL18, InsertReg<21, 5, TREG>, ?>;
class SBGP32_T: InstructionTemplate32<SBGP32, GPREL18, InsertReg<21, 5, TREG>, ?>;

class LHGP32_T: InstructionTemplate32<LHGP32, GPREL17_S1, InsertReg<21, 5, TREG>, ?>;
class LHUGP32_T: InstructionTemplate32<LHUGP32, GPREL17_S1, InsertReg<21, 5, TREG>, ?>;
class SHGP32_T: InstructionTemplate32<SHGP32, GPREL17_S1, InsertReg<21, 5, TREG>, ?>;

class BALC16_T: InstructionTemplate16<BALC16, PC10_S1, ?, ?>;
class BC16_T: InstructionTemplate16<BC16, PC10_S1, ?, ?>;

// -------------------------- End InstructionTemplate --------------------------

// ---------------------------- InstructionProperty ----------------------------

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

def LWGP32_P: InstructionProperty32<LWGP32, ExtractReg<21,5>, isRegValid, ?, ?>;
def SWGP32_P: InstructionProperty32<SWGP32, ExtractReg<21, 5>, isStoreSrcRegValid, ?, ?>;
def ADDIUGPW32_P: InstructionProperty32<ADDIUGPW32, ExtractReg<21, 5>, ?, ?, ?>;
def ADDIUGPB32_P: InstructionProperty32<ADDIUGPB32, ExtractReg<21, 5>, ?, ?, ?>;
def LBGP32_P: InstructionProperty32<LBGP32, ExtractReg<21, 5>, ?, ?, ?>;
def LBUGP32_P: InstructionProperty32<LBUGP32, ExtractReg<21,5>, ?, ?, ?>;
def SBGP32_P: InstructionProperty32<SBGP32, ExtractReg<21, 5>, ?, ?, ?>;
def LHGP32_P: InstructionProperty32<LHGP32, ExtractReg<21, 5>, ?, ?, ?>;
def LHUGP32_P: InstructionProperty32<LHUGP32, ExtractReg<21, 5>, ?, ?, ?>;
def SHGP32_P: InstructionProperty32<SHGP32, ExtractReg<21, 5>, ?, ?, ?>;

def BC16_P: InstructionProperty16NoConv<BC16, ?, ?>;
def BALC16_P: InstructionProperty16NoConv<BALC16, ?, ?>;
def BC32_P: InstructionProperty32NoValid<BC32, ?, ?>;
def BALC32_P: InstructionProperty32NoValid<BALC32, ?, ?>;
def BEQZC16_P: InstructionProperty16<BEQZC16, ExtractReg<7, 3>, convertReg, ?, ?>;
def BNEZC16_P: InstructionProperty16<BNEZC16, ExtractReg<7, 3>, convertReg, ?, ?>;

// Extract reg -> extracts count in save/restore
def RESTOREGP_P: InstructionProperty32<RESTOREGP, ExtractReg<16, 4>, ?, extractSaveResFields, ?>;
def RESTOREJRCGP_P: InstructionProperty32<RESTOREJRCGP, ExtractReg<16, 4>, ?, extractSaveResFields, ?>;
def SAVEGP_P: InstructionProperty32<SAVEGP, ExtractReg<16, 4>, ?, extractSaveResFields, ?>;

// -------------------------- End InstructionProperty --------------------------

// ---------------------------- TransformationType -----------------------------

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

def TT_GPREL16: TransformationType;
def TT_GPREL32_NMF: TransformationType;
def TT_GPREL_LONG: TransformationType;
def TT_GPREL_NMF: TransformationType;
def TT_GPREL32_WORD: TransformationType;

def TT_SAVERESTORE16: TransformationType;

// -------------------------- End TransformationType ---------------------------


// -------------------------- TransformationTemplate ---------------------------

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

def : TransformationTemplate<TT_ABS32_LONG, LWGP32_P, (relocs GPREL19_S2), (insns LUI_T<HI20, SREG>, LW32_T<LO12>)>;
def : TransformationTemplate<TT_PCREL_NMF, LWGP32_P, (relocs GPREL19_S2), (insns LWPC48_T<PC_I32>)>;
def : TransformationTemplate<TT_PCREL32_LONG, LWGP32_P, (relocs GPREL19_S2), (insns ALUIPC_T<PC_HI20, SREG>, LW32_T<LO12>)>;
def : TransformationTemplate<TT_GPREL16, LWGP32_P, (relocs GPREL19_S2), (insns LWGP16_T<>)>;
def : TransformationTemplate<TT_GPREL32_NMF, LWGP32_P, (relocs GPREL19_S2), (insns ADDIUGP48_T<GPREL_I32, SREG>, LW32_T<NONE>)>;
def : TransformationTemplate<TT_GPREL_LONG, LWGP32_P, (relocs GPREL19_S2), (insns LUI_T<GPREL_HI20, SREG>, ADDUGP_T<SREG, SREG>, LW32_T<GPREL_LO12>)>;

def : TransformationTemplate<TT_ABS32_LONG, SWGP32_P, (relocs GPREL19_S2), (insns LUI_T<HI20, SREG>, SW32_T<LO12>)>;
def : TransformationTemplate<TT_PCREL_NMF, SWGP32_P, (relocs GPREL19_S2), (insns SWPC48_T<>)>;
def : TransformationTemplate<TT_PCREL32_LONG, SWGP32_P, (relocs GPREL19_S2), (insns ALUIPC_T<PC_HI20, SREG>, SW32_T<LO12>)>;
def : TransformationTemplate<TT_GPREL16, SWGP32_P, (relocs GPREL19_S2), (insns SWGP16_T<>)>;
def : TransformationTemplate<TT_GPREL32_NMF, SWGP32_P, (relocs GPREL19_S2), (insns ADDIUGP48_T<GPREL_I32, SREG>, SW32_T<NONE>)>;
def : TransformationTemplate<TT_GPREL_LONG, SWGP32_P, (relocs GPREL19_S2), (insns LUI_T<GPREL_HI20, SREG>, ADDUGP_T<SREG, SREG>, SW32_T<GPREL_LO12>)>;

def : TransformationTemplate<TT_ABS32_LONG, ADDIUGPW32_P, (relocs GPREL19_S2), (insns LUI_T<HI20, SREG>, ORI_T<LO12, TREG, SREG>)>;
def : TransformationTemplate<TT_PCREL32_LONG, ADDIUGPW32_P, (relocs GPREL19_S2), (insns ALUIPC_T<PC_HI20, SREG>, ORI_T<LO12, TREG, SREG>)>;
def : TransformationTemplate<TT_GPREL_NMF, ADDIUGPW32_P, (relocs GPREL19_S2), (insns ADDIUGP48_T<GPREL_I32, TREG>)>;
def : TransformationTemplate<TT_GPREL_LONG, ADDIUGPW32_P, (relocs GPREL19_S2), (insns LUI_T<GPREL_HI20, SREG>, ORI_T<GPREL_LO12, SREG, SREG>, ADDUGP_T<SREG, TREG>)>;

def : TransformationTemplate<TT_ABS32_LONG, ADDIUGPB32_P, (relocs GPREL18), (insns LUI_T<HI20, SREG>, ORI_T<LO12, TREG, SREG>)>;
def : TransformationTemplate<TT_PCREL32_LONG, ADDIUGPB32_P, (relocs GPREL18), (insns ALUIPC_T<PC_HI20, SREG>, ORI_T<LO12, TREG, SREG>)>;
def : TransformationTemplate<TT_GPREL32_WORD, ADDIUGPB32_P, (relocs GPREL18), (insns ADDIUGPW32_T<>)>;
def : TransformationTemplate<TT_GPREL_NMF, ADDIUGPB32_P, (relocs GPREL18), (insns ADDIUGP48_T<GPREL_I32, TREG>)>;
def : TransformationTemplate<TT_GPREL_LONG, ADDIUGPB32_P, (relocs GPREL18), (insns LUI_T<GPREL_HI20, SREG>, ORI_T<GPREL_LO12, SREG, SREG>, ADDUGP_T<SREG, TREG>)>;

def : TransformationTemplate<TT_ABS32_LONG, LBGP32_P, (relocs GPREL18), (insns LUI_T<HI20, SREG>, LB32_T<LO12>)>;
def : TransformationTemplate<TT_PCREL32_LONG, LBGP32_P, (relocs GPREL18), (insns ALUIPC_T<PC_HI20, SREG>, LB32_T<LO12>)>;
def : TransformationTemplate<TT_GPREL32_NMF, LBGP32_P, (relocs GPREL18), (insns ADDIUGP48_T<GPREL_I32, SREG>, LB32_T<NONE>)>;
def : TransformationTemplate<TT_GPREL_LONG, LBGP32_P, (relocs GPREL18), (insns LUI_T<GPREL_HI20, SREG>, ADDUGP_T<SREG, SREG>, LB32_T<GPREL_LO12>)>;

def : TransformationTemplate<TT_ABS32_LONG, LBUGP32_P, (relocs GPREL18), (insns LUI_T<HI20, SREG>, LBU32_T<LO12>)>;
def : TransformationTemplate<TT_PCREL32_LONG, LBUGP32_P, (relocs GPREL18), (insns ALUIPC_T<PC_HI20, SREG>, LBU32_T<LO12>)>;
def : TransformationTemplate<TT_GPREL32_NMF, LBUGP32_P, (relocs GPREL18), (insns ADDIUGP48_T<GPREL_I32, SREG>, LBU32_T<NONE>)>;
def : TransformationTemplate<TT_GPREL_LONG, LBUGP32_P, (relocs GPREL18), (insns LUI_T<GPREL_HI20, SREG>, ADDUGP_T<SREG, SREG>, LBU32_T<GPREL_LO12>)>;

def : TransformationTemplate<TT_ABS32_LONG, SBGP32_P, (relocs GPREL18), (insns LUI_T<HI20, SREG>, SB32_T<LO12>)>;
def : TransformationTemplate<TT_PCREL32_LONG, SBGP32_P, (relocs GPREL18), (insns ALUIPC_T<PC_HI20, SREG>, SB32_T<LO12>)>;
def : TransformationTemplate<TT_GPREL32_NMF, SBGP32_P, (relocs GPREL18), (insns ADDIUGP48_T<GPREL_I32, SREG>, SB32_T<NONE>)>;
def : TransformationTemplate<TT_GPREL_LONG, SBGP32_P, (relocs GPREL18), (insns LUI_T<GPREL_HI20, SREG>, ADDUGP_T<SREG, SREG>, SB32_T<GPREL_LO12>)>;

def : TransformationTemplate<TT_ABS32_LONG, LHGP32_P, (relocs GPREL17_S1), (insns LUI_T<HI20, SREG>, LH32_T<LO12>)>;
def : TransformationTemplate<TT_PCREL32_LONG, LHGP32_P, (relocs GPREL17_S1), (insns ALUIPC_T<PC_HI20, SREG>, LH32_T<LO12>)>;
def : TransformationTemplate<TT_GPREL32_NMF, LHGP32_P, (relocs GPREL17_S1), (insns ADDIUGP48_T<GPREL_I32, SREG>, LH32_T<NONE>)>;
def : TransformationTemplate<TT_GPREL_LONG, LHGP32_P, (relocs GPREL17_S1), (insns LUI_T<GPREL_HI20, SREG>, ADDUGP_T<SREG, SREG>, LH32_T<GPREL_LO12>)>;

def : TransformationTemplate<TT_ABS32_LONG, LHUGP32_P, (relocs GPREL17_S1), (insns LUI_T<HI20, SREG>, LHU32_T<LO12>)>;
def : TransformationTemplate<TT_PCREL32_LONG, LHUGP32_P, (relocs GPREL17_S1), (insns ALUIPC_T<PC_HI20, SREG>, LHU32_T<LO12>)>;
def : TransformationTemplate<TT_GPREL32_NMF, LHUGP32_P, (relocs GPREL17_S1), (insns ADDIUGP48_T<GPREL_I32, SREG>, LHU32_T<NONE>)>;
def : TransformationTemplate<TT_GPREL_LONG, LHUGP32_P, (relocs GPREL17_S1), (insns LUI_T<GPREL_HI20, SREG>, ADDUGP_T<SREG, SREG>, LHU32_T<GPREL_LO12>)>;

def : TransformationTemplate<TT_ABS32_LONG, SHGP32_P, (relocs GPREL17_S1), (insns LUI_T<HI20, SREG>, SH32_T<LO12>)>;
def : TransformationTemplate<TT_PCREL32_LONG, SHGP32_P, (relocs GPREL17_S1), (insns ALUIPC_T<PC_HI20, SREG>, SH32_T<LO12>)>;
def : TransformationTemplate<TT_GPREL32_NMF, SHGP32_P, (relocs GPREL17_S1), (insns ADDIUGP48_T<GPREL_I32, SREG>, SH32_T<NONE>)>;
def : TransformationTemplate<TT_GPREL_LONG, SHGP32_P, (relocs GPREL17_S1), (insns LUI_T<GPREL_HI20, SREG>, ADDUGP_T<SREG, SREG>, SH32_T<GPREL_LO12>)>;

def : TransformationTemplate<TT_PCREL32, BC16_P, (relocs PC10_S1), (insns BC32_T<>)>;

def : TransformationTemplate<TT_PCREL32, BALC16_P, (relocs PC10_S1), (insns BALC32_T<>)>;

def : TransformationTemplate<TT_PCREL16, BC32_P, (relocs PC25_S1), (insns BC16_T<>)>;

def : TransformationTemplate<TT_PCREL16, BALC32_P, (relocs PC25_S1), (insns BALC16_T<>)>;

def : TransformationTemplate<TT_PCREL32, BEQZC16_P, (relocs PC7_S1), (insns BEQC32_T<>)>;

def : TransformationTemplate<TT_PCREL32, BNEZC16_P, (relocs PC7_S1), (insns BNEC32_T<>)>;

def : TransformationTemplate<TT_DISCARD, RESTOREGP_P, (relocs SAVERESTORE), (insns RESTORE32_T<>)>;

def : TransformationTemplate<TT_DISCARD, RESTOREJRCGP_P, (relocs SAVERESTORE), (insns RESTOREJRC32_T<>)>;
def : TransformationTemplate<TT_SAVERESTORE16, RESTOREJRCGP_P, (relocs SAVERESTORE), (insns RESTOREJRC16_T<>)>;

def : TransformationTemplate<TT_DISCARD, SAVEGP_P, (relocs SAVERESTORE), (insns SAVE32_T<>)>;
def : TransformationTemplate<TT_SAVERESTORE16, SAVEGP_P, (relocs SAVERESTORE), (insns SAVE16_T<>)>;


// ------------------------ End TransformationTemplate -------------------------