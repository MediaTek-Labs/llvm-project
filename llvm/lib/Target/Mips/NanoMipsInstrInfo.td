//=- NanoMipsInstrInfo.td - NanoMips Instruction Information -*- tablegen -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes NanoMips instructions.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Mips profiles and nodes
//===----------------------------------------------------------------------===//

def MipsFullAddr : SDNode<"MipsISD::FullAddr", SDTIntUnaryOp>;
def MipsFullAddrAdd : SDNode<"MipsISD::FullAddrAdd", SDTIntBinOp>;
def NMUnalignedLW : SDNode<"MipsISD::UALW", SDTMipsLoadLR, [SDNPHasChain, SDNPMayLoad, SDNPMemOperand]>;
def NMUnalignedLH : SDNode<"MipsISD::UALH", SDTMipsLoadLR, [SDNPHasChain, SDNPMayLoad, SDNPMemOperand]>;
def NMUnalignedSW : SDNode<"MipsISD::UASW", SDTStore, [SDNPHasChain, SDNPMayStore, SDNPMemOperand]>;
def NMUnalignedSH : SDNode<"MipsISD::UASH", SDTStore, [SDNPHasChain, SDNPMayStore, SDNPMemOperand]>;

def SDT_NMbr_jt : SDTypeProfile<0, 2, []>;

def NMbr_jt : SDNode<"MipsISD::BR_JT", SDT_NMbr_jt, [SDNPHasChain]>;

//===----------------------------------------------------------------------===//
// nanoMIPS Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//

def imm32_NM : IntImmLeaf<i32, [{
    return (uint32_t)Imm.getZExtValue() == (uint64_t)Imm.getZExtValue();
}]>;
def immFitsAddiu32 : IntImmLeaf<i32, [{
  int64_t N = Imm.getSExtValue();
  return (N > 4095 && N <= 65535);
}]>;
def immFitsAddiuNeg : IntImmLeaf<i32, [{
  int64_t N = Imm.getSExtValue();
  return (N < -256 && N >= -4095);
}]>;
def imm32ZExt12 : IntImmLeaf<i32, [{ return isUInt<12>(Imm.getZExtValue()); }]>;
def imm32ZExt12ANDI :
  IntImmLeaf<i32, [{ return isUInt<12>(Imm.getZExtValue()) ||
                            Imm.getZExtValue() == 65535; }]>;
def imm32ZExt5  : IntImmLeaf<i32, [{ return isUInt<5>(Imm.getZExtValue()); }]>;
def imm32ZExt3  : IntImmLeaf<i32, [{ return Imm.getZExtValue() > 0 &&
				  Imm.getZExtValue() <= 8;}]>;
def imm32SExt12  : IntImmLeaf<i32, [{ return isInt<12>(Imm.getSExtValue()); }]>;
def imm32Neg12  : IntImmLeaf<i32, [{ return isInt<13>(Imm.getSExtValue()) &&
				    Imm.getSExtValue() < 0 &&
				    Imm.getSExtValue() >= -4095; }]>;
// True if (N + 1) fits in 12-bit field.
def immZExt12Plus1 : PatLeaf<(imm), [{
  return isUInt<13>(N->getZExtValue()) && isUInt<12>(N->getZExtValue() + 1);
}]>;
def immZExt7Plus1 : PatLeaf<(imm), [{
  return isUInt<8>(N->getZExtValue()) && isUInt<7>(N->getZExtValue() + 1);
}]>;
// Immediate range for signed 20-bit upper part
def imm32SExt20s12 : IntImmLeaf<i32, [{ return isInt<32>(Imm.getSExtValue())
                                       && (Imm.getZExtValue() % 4096 == 0); }]>;

// Immediate range for 16-bit load immediate
def imm7M1To126 : IntImmLeaf<i32, [{
    return (Imm.getSExtValue() >= -1 &&  Imm.getSExtValue() < 127);
}]>;

// Immediate range for unsigned 18-bit
def imm32ZExt18 : IntImmLeaf<i32, [{ return isInt<18>(Imm.getZExtValue());}]>;

// Immediate range for unsigned 18-bit
def imm32ZExt4 : IntImmLeaf<i32, [{ return isInt<4>(Imm.getZExtValue());}]>;

// Immediate range for unsigned 21-bit word-aligned
def imm32ZExt21s2 : IntImmLeaf<i32, [{ return isInt<21>(Imm.getZExtValue()) &&
					      (Imm.getZExtValue() % 4 == 0); }]>;

// Immediate range for unsigned 21-bit word-aligned
def imm32ZExt8s2 : IntImmLeaf<i32, [{ return isInt<8>(Imm.getZExtValue()) &&
					     (Imm.getZExtValue() % 4 == 0); }]>;

// Immediate range for 16-bit AND immediate
def imm4Mask : IntImmLeaf<i32, [{
    return ((Imm.getZExtValue() < 11) ||
	   (Imm.getZExtValue() == 0xff) || (Imm.getZExtValue() == 0xffff) ||
	   (Imm.getZExtValue() == 0xe) ||  (Imm.getZExtValue() == 0xf));
}]>;

class ConstantSImmRangeAsmOperandClass<int Bottom, int Top,
                                       list<AsmOperandClass> Supers = []>
    : AsmOperandClass {
  let Name = "ConstantNegImmRange" # Bottom # "_" # Top;
  let RenderMethod = "addSImmOperands<" # 12 # ">";
  let PredicateMethod = "isConstantNegImmRange<" # Bottom # ", " # Top # ">";
  let SuperClasses = Supers;
  let DiagnosticType = "SImmRange" # Bottom # "_" # Top;
}

class NegImmAsmOperandClass<int Bits, list<AsmOperandClass> Supers = []>
    : AsmOperandClass {
  let Name = "NegImm" # Bits;
  let RenderMethod = "addSImmOperands<32>";
  let PredicateMethod = "isNegImm<" # Bits # ">";
  let SuperClasses = Supers;
}

class Imm7AsmOperandClass : AsmOperandClass {
  let Name = "UImm7N1";
  let RenderMethod = "addSImmOperands<8>";
  let PredicateMethod = "isConstantSImmRange<-1,126>";
}

class UImm4AsmOperandClass : AsmOperandClass {
  let Name = "UImm4Mask";
  let RenderMethod = "addUImmOperands<16>";
  let PredicateMethod = "isConstantUImmMask";
}

class UImm3ShiftAsmOperandClass : AsmOperandClass {
  let Name = "UImm3Shift";
  let RenderMethod = "addUImmOperands<4>";
  let PredicateMethod = "isConstantUImmRange<1,8>";
}

def UImm12AsmOperandClass : UImmAsmOperandClass<12, []>;
def UImm5AsmOperandClass : UImmAsmOperandClass<5, [UImm12AsmOperandClass]>;
def Imm7AsmOperand : Imm7AsmOperandClass;
def UImm4AsmOperand : UImm4AsmOperandClass;
def UImm3ShiftAsmOperand : UImm3ShiftAsmOperandClass;

def SImm12AsmOperandClass : SImmAsmOperandClass<12, []>;
def NImm12AsmOperandClass : NegImmAsmOperandClass<12, []>;
def NaImm12AsmOperandClass : ConstantSImmRangeAsmOperandClass<4095, 1, []>;

class SImmScaledAsmOperandClass<int Bits, int Align, list<AsmOperandClass> Supers = []>
    : AsmOperandClass{
  let Name = "SImm" # Bits # "s" # Align;
  let RenderMethod = "addSImmOperands<32>";
  let PredicateMethod = "isSImm<" # Bits # ">";
  let SuperClasses = Supers;
  let DiagnosticType = "SImm" # Bits # "s" # Align;
}

class NMUImmAsmOperandClass<int Bits,
			    list<AsmOperandClass> Supers = [],
			    int Offset = 0> :
			    AsmOperandClass {
  let Name = "NMUImm" # Bits # "_" # Offset;
  let RenderMethod = "addConstantUImmOperands<" # Bits # "," # Offset # ">";
  let PredicateMethod = "isConstantUImm<" # Bits # "," # Offset # ">";
  let SuperClasses = Supers;
}

class NMUImm16AsmOperandClass<list<AsmOperandClass> Supers = []> :
      AsmOperandClass {
  let Name = "NMUImm16";
  let RenderMethod = "addUImmOperands<16>";
  let PredicateMethod = "isLo12OrUImm16Offset";
  let SuperClasses = Supers;
}

class NMSImmAsmOperandClass<int Bits,
			    list<AsmOperandClass> Supers = [],
			    int Offset = 0> :
			    AsmOperandClass {
  let Name = "NMSImm" # Bits # "_" # Offset;
  let RenderMethod = "addSImmOperands<" # Bits # ">";
  let PredicateMethod = "isConstantSImm<" # Bits # "," # Offset # ">";
  let SuperClasses = Supers;
}

class NMUImmScaledAsmOperandClass<int Bits,
			          int Shift = 0,
				  list<AsmOperandClass> Supers = []> :
    AsmOperandClass {
  let Name = "NMUImm" # Bits # "_s" # Shift;
  let RenderMethod = "addConstantUImmOperands<32>";
  let PredicateMethod = "isScaledUImm<" # Bits # "," # Shift # ">";
  let SuperClasses = Supers;
}

class Hi20AsmOperandClass<string Suffix = ""> : AsmOperandClass {
  let Name = "Hi20Offset" # Suffix;
  let RenderMethod = "addSImmOperands<20>";
}

class SymAsmOperandClass<string Type = "", string Suffix = "",
			list<AsmOperandClass> Supers = []>
    : AsmOperandClass{
  let Name = "Sym" # Type # Suffix;
  let DiagnosticType = "Sym" # Type;
  let PredicateMethod = "isSym" # Type;
  let SuperClasses = Supers;
  let RenderMethod = "addSym" # Type # "Operands";
}

def SymAsmOperand : SymAsmOperandClass;

def SymAsmOperandGPRel18 : SymAsmOperandClass<"GPRel", "18"> {
  let PredicateMethod = "isSymGPRel<18>";
  let RenderMethod = "addSymGPRelOperands<18>";
}
def SymAsmOperandGPRel19s2 : SymAsmOperandClass<"GPRel", "19s2"> {
  let PredicateMethod = "isSymGPRel<19, 2>";
  let RenderMethod = "addSymGPRelOperands<21,0>";
}
def SymAsmOperandGPRel : SymAsmOperandClass<"GPRel">;

def UImm18AsmOperand : NMUImmAsmOperandClass<18>;

def uimm18_nm : Operand<i32> {
  let ParserMatchClass = UImm18AsmOperand;
}

def uimm12_nm : Operand<i32> {
  let PrintMethod = "printUImm<12>";
  let ParserMatchClass = NMUImmAsmOperandClass<12>;
}

def uimm12s3_nm : Operand<i32> {
  let PrintMethod = "printUImm<12>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<9, 3, []>;
}
def uimm8s4_nm : Operand<i32> {
  let PrintMethod = "printUImm<8>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<4, 4, [uimm12s3_nm.ParserMatchClass]>;
}
def uimm3plus1_nm : Operand<i32> {
  let ParserMatchClass = NMUImmAsmOperandClass<3, [], 1>;
}
def nimm12_nm : Operand<i32> {
  let ParserMatchClass = !cast<AsmOperandClass>("NImm12AsmOperandClass");
  let EncoderMethod = "getNegImm12Encoding";
  let DecoderMethod = "DecodeNegImm12";
}
def uimm16_nm : Operand<i32> {
  let PrintMethod = "printUImm<16>";
  let ParserMatchClass = NMUImm16AsmOperandClass<>;
}
def uimm5s1_nm : Operand<i32> {
  let PrintMethod = "printUImm<5>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<4, 1>;
}
def uimm5s2_nm : Operand<i32> {
  let PrintMethod = "printUImm<5>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<3, 2, [uimm16_nm.ParserMatchClass]>;
}
def uimm8s2_nm : Operand<i32> {
  let PrintMethod = "printUImm<8>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<6, 2, [uimm16_nm.ParserMatchClass]>;
  let DecoderMethod = "DecodeUImmWithReg<8, 0, 1, Mips::SP_NM>";
}
def nimm7_nm : Operand<i32> {
  let ParserMatchClass = !cast<AsmOperandClass>("Imm7AsmOperand");
  let DecoderMethod = "DecodeImmM1To126";
}
def uimm4mask_nm : Operand<i32> {
  let EncoderMethod = "getUImm4MaskEncoding";
  let PrintMethod = "printUImm<16>";
  let ParserMatchClass = !cast<AsmOperandClass>("UImm4AsmOperand");
  let DecoderMethod = "DecodeUImm4Mask";
}
def uimm3shift_nm : Operand<i32> {
  let EncoderMethod = "getUImm3ShiftEncoding";
  let PrintMethod = "printUImm<4>";
  let ParserMatchClass = !cast<AsmOperandClass>("UImm3ShiftAsmOperand");
  let DecoderMethod = "DecodeUImm3Shift";
}

def simm32_nm : Operand<i32> {
  let ParserMatchClass = NMSImmAsmOperandClass<32>;
  let DecoderMethod = "DecodeSimm32";
  let EncoderMethod = "getSImm32Encoding";
}

def simm32s12_nm : Operand<i32> {
  let ParserMatchClass = Hi20AsmOperandClass<>;
  let PrintMethod = "printHi20";
}

def simm32s12_pcrel_nm : Operand<i32> {
  let ParserMatchClass = Hi20AsmOperandClass<"PCRel">;
  let PrintMethod = "printHi20PCRel";
}

def sym32_pc_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymPCRel";
  let ParserMatchClass = SymAsmOperand;
}

def sym32_gp_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymGPRel";
  let ParserMatchClass = SymAsmOperandGPRel;
  let DecoderMethod = "DecodeSImmWithReg<32, 0, 1, Mips::GP_NM>";
}

def sym32_gp18_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymGPRel";
  let ParserMatchClass = SymAsmOperandGPRel18;
  let DecoderMethod = "DecodeUImmWithReg<18, 0, 1, Mips::GP_NM>";
}

def sym32_gp19s2_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymGPRel";
  let ParserMatchClass = SymAsmOperandGPRel19s2;
  let DecoderMethod = "DecodeUImmWithReg<21, 0, 1, Mips::GP_NM>";
}

def sym32_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymAbs";
  let ParserMatchClass = SymAsmOperand;
}

def simm32power2 : IntImmLeaf<i32, [{ return isPowerOf2_32(Imm.getSExtValue()); }]>;

def addrsimm9 : ComplexPattern<iPTR, 2, "selectIntAddrSImm9", [frameindex]>;
def addruimm12 : ComplexPattern<iPTR, 2, "selectIntAddrUImm12", [frameindex]>;
def addrindexed : ComplexPattern<iPTR, 2, "selectIntAddrIndexed", [frameindex]>;
def addrindexedlsl1 : ComplexPattern<iPTR, 2, "selectIntAddrIndexedLsl1", [frameindex]>;
def addrindexedlsl2 : ComplexPattern<iPTR, 2, "selectIntAddrIndexedLsl2", []>;
def addruimm6s2 : ComplexPattern<iPTR, 2, "selectIntAddrUImm6s2", [frameindex]>;
def addruimm3s1 : ComplexPattern<iPTR, 2, "selectIntAddrUImm3s1", [frameindex]>;
def addruimm2 : ComplexPattern<iPTR, 2, "selectIntAddrUImm2", [frameindex]>;
def addruimm4s2 : ComplexPattern<iPTR, 2, "selectIntAddrUImm4s2", [frameindex]>;
def addrgp19s2 : ComplexPattern<iPTR, 2, "selectIntAddrUImm19s2", [frameindex]>;
def addrgp18 : ComplexPattern<iPTR, 2, "selectIntAddrUImm18", [frameindex]>;
def addrgp17s1 : ComplexPattern<iPTR, 2, "selectIntAddrUImm17s1", [frameindex]>;
def addrgp7s2 : ComplexPattern<iPTR, 2, "selectIntAddrUImm7s2", [frameindex]>;
def addrsp5s2 : ComplexPattern<iPTR, 2, "selectIntAddrUImm5s2", [frameindex]>;
def addrsym : ComplexPattern<iPTR, 1, "selectAddrSym", [frameindex]>;
def addrgprel : ComplexPattern<iPTR, 1, "selectAddrSymGPRel", [frameindex]>;
def offsetgp19s2 : ComplexPattern<iPTR, 1, "selectOffsetGP19s2", [frameindex]>;
def offsetgp18 : ComplexPattern<iPTR, 1, "selectOffsetGP18", [frameindex]>;

def Log2XForm : SDNodeXForm<imm, [{ return CurDAG->getTargetConstant(Log2_32(N->getZExtValue()),
                                                                   SDLoc(N), MVT::i32);
                                  }]>;

class RegConstraint<string C> {
  string Constraints = C;
}
class NoEncode<string E> {
  string DisableEncoding = E;
}

//===----------------------------------------------------------------------===//
//
// Instruction Descriptions
//
//===----------------------------------------------------------------------===//


class _Pool_P32<bits<5> psel> : InstSize32 {
  let Inst{31-26} = {psel{4...2}, 0b0, psel{1...0}};
}

class _Pool_P_ADDIU<bits<5> psel> : _Pool_P32<0b00000> {
  let Inst{25...21} = psel;
}

class _Pool_P32A<bits<3> psel> : _Pool_P32<0b00100> {
  let Inst{2...0} = psel;
}

class _Pool_P_GP_W<bits<2> psel> : _Pool_P32<0b01000> {
  let Inst{1...0} = psel;
}

class _Pool_P_U12<bits<4> psel> : _Pool_P32<0b10000> {
  let Inst{15-12} = psel;
}

class _Pool_P_LUI<bits<1> psel> : _Pool_P32<0b11100> {
  let Inst{1} = psel;
}

class _Pool_P48I<bits<5> psel> : InstSize48 {
  let Inst{47...42} = 0b011000;
  let Inst{36...32} = psel;
}

class _Pool_P_GP_BH<bits<3> psel> : _Pool_P32<0b01001> {
  let Inst{20...18} = psel;
}

class _Pool_P_LS_U12<bits<4> psel> : _Pool_P32<0b10001> {
  let Inst{15...12} = psel;
}

class _Pool_P_LS_S9<bits<3> psel> : _Pool_P32<0b10101> {
  let Inst{10...8} = psel;
}

// TODO MOVE.BALC

class _Pool_P_BAL<bits<1> psel> : _Pool_P32<0b00110> {
  let Inst{25} = psel;
}

class _Pool_P_J<bits<4> psel> : _Pool_P32<0b01010> {
  let Inst{15...12} = psel;
}

class _Pool_P_BR1<bits<2> psel> : _Pool_P32<0b10010> {
  let Inst{15...14} = psel;
}

class _Pool_P_BR2<bits<2> psel> : _Pool_P32<0b10110> {
  let Inst{15...14} = psel;
}

class _Pool_P_BR12<bits<3> psel> : _Pool_P32<0b10010> {
  let Inst{29} = psel{2};
  let Inst{15...14} = psel{1...0};
}

class _Pool_P_BRI<bits<3> psel> : _Pool_P32<0b11010> {
  let Inst{20...18} = psel;
}

class _Pool_P_BALRSC<bits<5> psel> : _Pool_P_J<0b1000> {
  let Inst{25...21} = psel;
}

class _Pool_P_SHIFT<bits<4> psel> : _Pool_P_U12<0b1100> {
  let Inst{8...5} = psel;
  bits<5> shift;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{4...0} = shift;
}

class _Pool_P_SLL<bits<5> rt, bits<5> shift> : _Pool_P_SHIFT<0b0000> {
  let Inst{25...21} = rt;
  let Inst{4...0} = shift;
}

class _Pool_P_ROTX<bits<2> psel> : _Pool_P_U12<0b1101> {
  let Inst{11} = psel{1};
  let Inst{5} = psel{0};
}

class _Pool_P_EXT_INS<bits<4> psel> : _Pool_P_U12<psel> {
  let Inst{11} = 0;
  let Inst{5} = 0;
}

class _Pool_P_PREF_U12<bits<5> psel> : _Pool_P_LS_U12<0b0011> {
  let Inst{25...21} = psel;
}

class _Pool_P_LS_S0<bits<4> psel>: _Pool_P_LS_S9<0b000> {
  let Inst{14...11} = psel;
}

class _Pool_P_PREF_S9<bits<5> psel>: _Pool_P_LS_S0<0b0011> {
  let Inst{25...21} = psel;
}

class _Pool_P_LS_S1<bits<4> psel>: _Pool_P_LS_S9<0b001> {
  let Inst{14...11} = psel;
}

class _Pool_P_LL<bits<2> psel>: _Pool_P_LS_S1<0b1010> {
  let Inst{1...0} = psel;
}

class _Pool_P_SC<bits<2> psel>: _Pool_P_LS_S1<0b1011> {
  let Inst{1...0} = psel;
}

class _Pool_P_LS_E0<bits<4> psel>: _Pool_P_LS_S9<0b010> {
  let Inst{14...11} = psel;
}

class _Pool_P_LLE<bits<2> psel>: _Pool_P_LS_E0<0b1010> {
  let Inst{1...0} = psel;
}

class _Pool_P_PREFE<bits<5> psel>: _Pool_P_LS_E0<0b0011> {
  let Inst{25...21} = psel;
}

class _Pool_P_SCE<bits<2> psel>: _Pool_P_LS_E0<0b1011> {
  let Inst{1...0} = psel;
}

class _Pool_P_LS_WM<bits<1> psel>: _Pool_P_LS_S9<0b100> {
  let Inst{11} = psel;
}

class _Pool_P_LS_UAWM<bits<1> psel>: _Pool_P_LS_S9<0b101> {
  let Inst{11} = psel;
}

class _Pool_P16<bits<5> psel> : InstSize16 {
  let Inst{15-10} = {psel{4...2}, 0b1, psel{1...0}};
}

class _Pool_P16_MV : _Pool_P16<0b00000>;

class _Pool_P16_SR<bits<1> psel> : _Pool_P16<0b00011> {
  let Inst{8} = psel;
}

class _Pool_P16_SHIFT<bits<1> isel> : _Pool_P16<0b00100> {
  let Inst{3} = isel;
}

class _Pool_P16_4X4<bits<2> psel> : _Pool_P16<0b00111> {
  let Inst{8} = psel{1};
  let Inst{3} = psel{0};
}

class _Pool_P16C<bits<1> psel> : _Pool_P16<0b01000> {
  let Inst{0} = psel;
}

class _Pool_POOL16C_00<bits<2> psel> : _Pool_P16<0b01000> {
  let Inst{3...2} = psel;
  let Inst{1...0} = 0b00;
}

class _Pool_P16_LB<bits<2> psel> : _Pool_P16<0b01011> {
  let Inst{3...2} = psel;
}

class _Pool_P16_A1<bits<1> psel> : _Pool_P16<0b01100> {
  let Inst{6} = psel;
}

class _Pool_P16_LH<bits<2> psel> : _Pool_P16<0b01111> {
  let Inst{3} = psel{1};
  let Inst{0} = psel{0};
}

class _Pool_P16_A2<bits<1> psel> : _Pool_P16<0b10000> {
  let Inst{3} = psel;
}

class _Pool_P16_ADDU<bits<1> psel> : _Pool_P16<0b10100> {
  let Inst{0} = psel;
}

class _Pool_P16_BR<bits<4> psel> : _Pool_P16<0b11010> {
  let Inst{3...0} = psel;
}

class _Pool_P16_RI<bits<2> psel> : _Pool_P16_MV {
  let Inst{4...3} = psel;
  let Inst{9...5} = 0b00000;
}

class _Pool_P16_SYSCALL<bits<1> psel> : _Pool_P16_RI<0b01> {
  let Inst{2} = psel;
}

class _Pool_P_RI<bits<2> psel> : _Pool_P_ADDIU<0b00000> {
  let Inst{20...19} = psel;
}

class _Pool_POOL32A0<bits<1> psel> : _Pool_P32A<0b000> {
  let Inst{5} = psel;
}

class _Pool_POOL32A0_0<bits<6> psel> : _Pool_POOL32A0<0b0> {
  let Inst{9...6} = psel{5...2};
  let Inst{4...3} = psel{1...0};
}

class _Pool_P_TRAP<bits<1> isel> : _Pool_POOL32A0_0<0b000000>
{
  let Inst{10} = isel;
}

class _Pool_P_CMOVE<bits<1> isel> : _Pool_POOL32A0_0<0b100010>
{
  let Inst{10} = isel;
}

class _Pool_POOL32A7<bits<3> psel> : _Pool_P32A<0b111> {
  let Inst{5...3} = psel;
}

class _Pool_POOL32Axf_4<bits<7> isel> : _Pool_POOL32A7<0b111> {
  let Inst{15...9} = isel;
  let Inst{8...6} = 0b100;
}

class _Pool_POOL32Axf_5<bits<2> psel> : _Pool_POOL32A7<0b111> {
  let Inst{8...6} = 0b101;
  let Inst{15...14} = psel;
}

class _Pool_POOL32Axf_5_group1<bits<5> isel> : _Pool_POOL32Axf_5<0b01> {
  let Inst{13...9} = isel;
}

class _Pool_POOL32Axf_5_group3<bits<5> isel> : _Pool_POOL32Axf_5<0b11> {
  let Inst{13...9} = isel;
}

class _Pool_PP_LSX<bit scaled, bits<4> isel> : _Pool_POOL32A7<0b000> {
  let Inst{6} = scaled;
  let Inst{10...7} = isel;
}

// 16-bit arithmetic and logical instructions with 4x4 register operands, P16_4X4
class Arith4x4_Desc<string opstr, RegisterOperand RO,
			SDPatternOperator OpNode = null_frag>:
    InstNM<(outs RO:$dst), (ins RO:$rt, RO:$rs),
	!strconcat(opstr, "\t$dst, $rt, $rs"),
	   [(set RO:$dst, (OpNode RO:$rt, RO:$rs))]> {
  Format Form = FrmR;
}

class Arith4x4_Enc<bits<2> isel>:  _Pool_P16_4X4<isel> {
      bits<5> dst;
      bits<5> rs;
      bits<5> rt;

      let Inst{9} = dst{4};
      let Inst{7...5} = dst{2...0};
      let Inst{4} = rs{4};
      let Inst{2...0} = rs{2...0};
}

// 32-bit arithmetic and logical instructions with 3 register operands.
class ArithLogicR32_Desc<string opstr, RegisterOperand RO,
		    SDPatternOperator OpNode = null_frag>:
   InstNM<(outs RO:$rd), (ins RO:$rs, RO:$rt),
	 !strconcat(opstr, "\t$rd, $rs, $rt"),
	 [(set RO:$rd, (OpNode RO:$rs, RO:$rt))]>;

class ArithLogicR32_Enc<bits<6> psel>: _Pool_POOL32A0_0<psel> {
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
}

// 16-bit arithmetic and logical instructions with 2 register operands, POOL16C_0
class ArithLogicR16_Desc<string opstr, RegisterOperand RO,
		    SDPatternOperator OpNode = null_frag>:
    InstNM<(outs RO:$dst), (ins RO:$rs, RO:$rt),
	!strconcat(opstr, "\t$dst, $rs, $rt"),
	   [(set RO:$dst, (OpNode RO:$rs, RO:$rt))]>;

class ArithLogicR16U_Desc<string opstr, RegisterOperand RO,
				 SDPatternOperator OpNode = null_frag>:
    InstNM<(outs RO:$rt), (ins RO:$rs),
	!strconcat(opstr, "\t$rt, $rs"),
	   [(set RO:$rt, (OpNode RO:$rs))]> {
}

class ArithLogicR16_Enc<bits<2> isel>: _Pool_POOL16C_00<isel> {
  bits<3> rs;
  bits<3> rt;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
}

class CLZO_Enc<bits<7> isel>: _Pool_POOL32Axf_4<isel> {
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
}

class LogicU32_Enc<bits<6> isel>: _Pool_POOL32A0_0<isel> {
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
}

// 16-bit arithmetic instructions with 2 register operands, POOL16C_0
class AddSubR16_Desc<string opstr, RegisterOperand RO,
		    SDPatternOperator OpNode = null_frag>:
    InstNM<(outs RO:$rd), (ins RO:$rs, RO:$rt), !strconcat(opstr, "\t$rd, $rs, $rt"),
	   [(set RO:$rd, (OpNode RO:$rs, RO:$rt))]>;

class AddSubR16_Enc<bits<1> isel>: _Pool_P16_ADDU<isel> {
  bits<3> rd;
  bits<3> rs;
  bits<3> rt;
  let Inst{15-10} = 0b101100;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
  let Inst{3...1} = rd;
}

class ArithLogicINM2Reg<string opstr, Operand Od, RegisterOperand DRO,
		        RegisterOperand SRO,
		        SDPatternOperator imm_type = null_frag,
		        SDPatternOperator OpNode = null_frag> :
  InstNM<(outs DRO:$rt), (ins SRO:$rs, Od:$imm),
	 !strconcat(opstr, "\t$rt, $rs, $imm"),
	 [(set DRO:$rt, (OpNode SRO:$rs, imm_type:$imm))]>;

// Arithmetic and logical instructions with 2 register operands and immediate.
class ArithLogicINM<string opstr, Operand Od, RegisterOperand RO,
		    SDPatternOperator imm_type = null_frag,
		    SDPatternOperator OpNode = null_frag> :
  ArithLogicINM2Reg<opstr, Od, RO, RO, imm_type, OpNode>;

class ArithLogicI32<string opstr, Operand Od, RegisterOperand RO,
		    SDPatternOperator imm_type = null_frag,
		    SDPatternOperator OpNode = null_frag> :
  ArithLogicINM<opstr, Od, RO, imm_type, OpNode>;

class AddImmediate<string opstr, Operand Od, RegisterOperand DRO,
		   RegisterOperand SRO,
		   SDPatternOperator imm_type = null_frag,
		   SDPatternOperator OpNode = null_frag> :
  ArithLogicINM2Reg<opstr, Od, DRO, SRO, imm_type, OpNode>;

class LoadUpperI32<string opstr, Operand Od, RegisterOperand RO,
		   SDPatternOperator imm_type = null_frag,
		   SDPatternOperator OpNode = null_frag> :
  ArithLogicINM<opstr, Od, RO, imm_type, OpNode>, _Pool_P32<0b00000> {
  bits<5> rt;
  bits<5> rs;
  bits<16> imm;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...0} = imm;
}

class DivMod<string opstr, SDPatternOperator OpNode = null_frag> :
    ArithLogicR32_Desc<opstr, GPRNM32Opnd, OpNode> {
  // Need to insert TEQ, because DIV(U)/MOD(U) don't trap on division by zero.
  bit usesCustomInserter = 1;
}

class CondLogicI32_Desc<string opstr, Operand Od, RegisterOperand RO,
                          SDPatternOperator imm_type = null_frag,
			  SDPatternOperator OpNode = null_frag> :
  ArithLogicINM<opstr, Od, RO, imm_type, OpNode>;

class CondLogicI32_Enc<bits<4> psel> : _Pool_P_U12<psel> {
  bits<5> rt;
  bits<5> rs;
  bits<12> imm;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{11...0} = imm;
}

class ShiftI_Desc<string opstr, Operand Od, RegisterOperand RO,
                    SDPatternOperator imm_type = null_frag,
                    SDPatternOperator OpNode = null_frag> :
  ArithLogicINM<opstr, Od, RO, imm_type, OpNode>;

class ShiftI32_Enc<bits<4> psel> : _Pool_P_SHIFT<psel> {
  bits<5> rt;
  bits<5> rs;
  bits<5> imm;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{4...0} = imm;
}

class ShiftI16_Enc<bits<1> isel> : _Pool_P16_SHIFT<isel> {
  bits<3> rt;
  bits<3> rs;
  bits<3> imm;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
  let Inst{2...0} = imm;
}

class ExtBaseNM<RegisterOperand RO = GPRNM32Opnd> :
  InstNM<(outs RO:$rt), (ins RO:$rs, uimm5:$pos, uimm5_plus1:$size),
         "ext\t$rt, $rs, $pos, $size",
         [(set RO:$rt, (MipsExt RO:$rs, immZExt5:$pos, immZExt5Plus1:$size))],
         II_EXT>;

class InsBaseNM<RegisterOperand RO = GPRNM32Opnd> :
  InstNM<(outs RO:$rt),
         (ins RO:$rs, uimm5:$pos, uimm5_inssize_plus1:$size, RO:$src),
         "ins \t$rt, $rs, $pos, $size",
         [(set RO:$rt,
               (MipsIns RO:$rs, immZExt5:$pos, immZExt5Plus1:$size, RO:$src))],
         II_INS>, RegConstraint<"$src = $rt">;

class ExtInsBase_Enc<bits<4> isel> : _Pool_P_EXT_INS<isel> {
  bits<5> rt;
  bits<5> rs;
  bits<5> pos;
  bits<5> size;

  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{4...0} = pos;
  let Inst{10...6} = size;
}

class Trap<string opstr> :
  InstNM<(outs), (ins GPRNM32Opnd:$rs, GPRNM32Opnd:$rt, uimm5:$imm),
         !strconcat(opstr, "\t$rt, $rs, $imm"), []>, InstSize32 {
   let hasSideEffects = 1;
   let isCTI = 1;
 }

// Instructions with 1 register (dest) and 1 immediate operand.
class RegImmNM<string opstr, Operand Od, RegisterOperand RO,
               SDPatternOperator imm_type = null_frag,
               SDPatternOperator OpNode = null_frag> :
  InstNM<(outs RO:$rt), (ins Od:$imm), !strconcat(opstr, "\t$rt, $imm"),
         [(set RO:$rt, (OpNode imm_type:$imm))]>;

class LoadMemoryNM<string opstr, DAGOperand RO, DAGOperand MO,
                   SDPatternOperator OpNode = null_frag,
                   ComplexPattern Addr = addr,
                   InstrItinClass itin = NoItinerary> :
    InstNM<(outs RO:$rt), (ins MO:$addr), !strconcat(opstr, "\t$rt, $addr"),
           [(set RO:$rt, (OpNode Addr:$addr))],
           itin> {
  let DecoderMethod = "DecodeMem";
  let canFoldAsLoad = 1;
  let mayLoad = 1;
  string BaseOpcode = opstr;
}

class StoreMemoryNM<string opstr, DAGOperand RO, DAGOperand MO,
                    SDPatternOperator OpNode = null_frag,
                    ComplexPattern Addr = addr,
                    InstrItinClass itin> :
    InstNM<(outs), (ins RO:$rt, MO:$addr), !strconcat(opstr, "\t$rt, $addr"),
           [(OpNode RO:$rt, Addr:$addr)],
           itin> {
  let DecoderMethod = "DecodeMem";
  let mayStore = 1;
  string BaseOpcode = opstr;
}

class StoreMemoryNM4x4<string opstr, RegisterOperand RO, DAGOperand MO,
                       SDPatternOperator OpNode = null_frag,
                       ComplexPattern Addr = addr,
                       InstrItinClass itin> :
    InstNM<(outs), (ins GPRNM4ZOpnd:$rt, MO:$addr), !strconcat(opstr, "\t$rt, $addr"),
           [(OpNode GPRNM4ZOpnd:$rt, Addr:$addr)],
           itin> {
  let mayStore = 1;
  string BaseOpcode = opstr;
}

class Op48_Addr_Enc<bits<5> isel> : _Pool_P48I<isel> {
  bits<5> rt;
  bits<32> addr;
  let Inst{41...37} = rt;
  let Inst{31...16} = addr{15...0};
  let Inst{15...0} = addr{31...16};
}

class LoadPCBase<DAGOperand RO, DAGOperand MO> :
     InstNM<(outs RO:$rt), (ins MO:$addr), "lwpc\t$rt, $addr", [], II_LW>,
     InstSize48 {
  let mayLoad = 1;
}

class StorePCBase<DAGOperand RO, DAGOperand MO> :
    InstNM<(outs), (ins RO:$rt, MO:$addr), "swpc\t$rt, $addr", [], II_SW>,
    InstSize48 {
  let mayStore = 1;
}

class CallNM<string opstr,
             SDPatternOperator OpNode = null_frag,
             SDPatternOperator imm_type = null_frag> :
  InstNM<(outs), (ins calltarget:$addr), !strconcat(opstr, "\t$addr"),
         [(OpNode imm_type:$addr)]> {
  let DecoderMethod = "DecodeJumpTarget";
  let isCall = 1;
  let isCTI = 1;
  let Defs = [RA_NM];
}

class MoveBalcBase : InstNM<(outs GPRNM32Opnd:$rd),
                            (ins GPRNM32Opnd:$rt, calltarget:$addr),
                            "move.balc\t$rd, $rt, $addr", []>, InstSize32 {
  let isCall = 1;
  let isCTI = 1;
  let Defs = [RA_NM];
}

// Indirect branch instruction.
class IndirectBranchNM<string opstr, RegisterOperand RO,
                       SDPatternOperator operator = null_frag>:
    InstNM<(outs), (ins RO:$rs), !strconcat(opstr, "\t$rs"),
           [(operator RO:$rs)]>, InstSize16 {
  let isTerminator = 1;
  let isBarrier = 1;
  let isCTI = 1;
  let isBranch = 1;
  let isIndirectBranch = 1;
}

class AdjustStack<SDPatternOperator operator> :
    PseudoInstNM<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                 [(operator timm:$amt1, timm:$amt2)]> {
  let Defs = [SP_NM];
  let Uses = [SP_NM];
  let hasSideEffects = 1;
  let hasNoSchedulingInfo = 1;
}

def nmreglist : Operand<i32> {
  let PrintMethod = "printNanoMipsRegisterList";
}

class SaveRestore<string opstr> :
  InstNM<(outs),
         (ins uimm12_nm:$adj, nmreglist:$regs, variable_ops),
         !strconcat(opstr, "\t$adj$regs"), []> {
  let Defs = [SP_NM];
  let Uses = [SP_NM];
  let hasSideEffects = 1;
  let isCodeGenOnly = 1;
}
def SAVE_NM : SaveRestore<"save">, InstSize32;
def RESTORE_NM : SaveRestore<"restore">, InstSize32;
def RESTOREJRC_NM : SaveRestore<"restore.jrc">, InstSize32;

class LoadMultipleNM<string opstr> :
    InstNM<(outs GPRNM32Opnd:$rt), (ins mem:$addr, uimm3plus1_nm:$count),
           !strconcat(opstr, "\t$rt, $addr, $count"), []>, InstSize32 {
  let DecoderMethod = "DecodeMem";
  let mayLoad = 1;
}
class StoreMultipleNM<string opstr> :
    InstNM<(outs), (ins GPRNM32Opnd:$rt, mem:$addr, uimm3plus1_nm:$count),
           !strconcat(opstr, "\t$rt, $addr, $count"), []>, InstSize32 {
  let DecoderMethod = "DecodeMem";
  let mayStore = 1;
}

class EffectiveAddressNM<string opstr, RegisterOperand RO> :
  InstNM<(outs RO:$rt), (ins mem_ea:$addr), !strconcat(opstr, "\t$rt, $addr"),
         [(set RO:$rt, addr:$addr)]> {
  let isCodeGenOnly = 1;
  let hasNoSchedulingInfo = 1;
  let DecoderMethod = "DecodeMem";
  string BaseOpcode = !strconcat(opstr, "_lea");
}

class UnalignedLoad<string opstr, SDNode OpNode,
                    RegisterOperand RO = GPRNM32Opnd> :
    InstNM<(outs RO:$rt), (ins mem_simm9:$addr, RO:$src),
           !strconcat(opstr, "\t$rt, $addr"),
           [(set RO:$rt, (OpNode addrsimm9:$addr, RO:$src))]>, InstSize32 {
  let DecoderMethod = "DecodeMem";
  string Constraints = "$src = $rt";
  let mayLoad = 1;
  let canFoldAsLoad = 1;
}

class UnalignedStore<string opstr, SDNode OpNode,
                     RegisterOperand RO = GPRNM32Opnd> :
    InstNM<(outs), (ins RO:$rt, mem_simm9:$addr),
           !strconcat(opstr, "\t$rt, $addr"),
           [(OpNode RO:$rt, addrsimm9:$addr)]>, InstSize32 {
  let DecoderMethod = "DecodeMem";
  let mayStore = 1;
}

class ReadHardwareNM<RegisterOperand CPURegOperand, RegisterOperand RO> :
  InstNM<(outs CPURegOperand:$rt), (ins RO:$hs, uimm5:$sel),
  "rdhwr\t$rt, $hs, $sel", []>, InstSize32 {
  let isCodeGenOnly = 1;
}

class SignExtInRegNM<string opstr, ValueType vt, RegisterOperand RO> :
  InstNM<(outs RO:$rt), (ins RO:$rs), !strconcat(opstr, "\t$rt, $rs"),
         [(set RO:$rt, (sext_inreg RO:$rs, vt))]>;

class LoadScaledAddressNM<string opstr, RegisterOperand RO> :
  InstNM<(outs RO:$rd), (ins RO:$rs, RO:$rt, uimm2:$shift),
         !strconcat(opstr, "\t$rd, $rs, $rt, $shift"),
         [(set RO:$rd, (add (shl RO:$rs, immZExt2:$shift), RO:$rt))]>;

class LoadScaledAddress_Enc: _Pool_POOL32A7<0b001> {
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  bits<2> shift;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
  let Inst{10...9} = shift;
}

class CountLeading0NM<string opstr, RegisterOperand RO>:
  InstNM<(outs RO:$rt), (ins RO:$rs), !strconcat(opstr, "\t$rt, $rs"),
         [(set RO:$rt, (ctlz RO:$rs))]>;

class CountLeading1NM<string opstr, RegisterOperand RO>:
  InstNM<(outs RO:$rt), (ins RO:$rs), !strconcat(opstr, "\t$rt, $rs"),
         [(set RO:$rt, (ctlz (not RO:$rs)))]>;

class ReverseNM<string opstr, RegisterOperand RO, SDNode OpNode>:
  InstNM<(outs RO:$rd), (ins RO:$rs), !strconcat(opstr, "\t$rd, $rs"),
         [(set RO:$rd, (OpNode RO:$rs))]>;

class LoadImmediateNM<string opstr, RegisterOperand RO, Operand Od,
			SDPatternOperator imm_type = null_frag> :
  InstNM<(outs RO:$rt), (ins Od:$eu),
         !strconcat(opstr, "\t$rt, $eu"),
         [(set RO:$rt, imm_type:$eu)]>;

class LoadAddressPCRelNM<string opstr, RegisterOperand RO,
			 DAGOperand MO, ComplexPattern Addr> :
  InstNM<(outs RO:$rt), (ins MO:$addr),
         !strconcat(opstr, "\t$rt, $addr"),
         [(set RO:$rt, Addr:$addr)]>;

class LoadAddressGPRelNM<string opstr, RegisterOperand RO, DAGOperand MO, ComplexPattern Addr> :
  InstNM<(outs RO:$rt), (ins RO:$rs, MO:$addr),
         !strconcat(opstr, "\t$rt, $rs, $addr"),
         [(set RO:$rt, (add RO:$rs, Addr:$addr))]>;

class LoadAddressAbsNM<string opstr, RegisterOperand RO, DAGOperand MO, ComplexPattern Addr> :
  InstNM<(outs RO:$rt), (ins MO:$imm),
         !strconcat(opstr, "\t$rt, $imm"),
         [(set RO:$rt, Addr:$imm)]>;

//===----------------------------------------------------------------------===//
//
// Instruction Definitions
//
//===----------------------------------------------------------------------===//


// 16-bit register operations
def NOT16_NM : ArithLogicR16U_Desc<"not", GPRNM16R3Opnd, not>,
	       ArithLogicR16_Enc<0b00>;
let isCommutable=1 in {
def XOR16_NM : ArithLogicR16_Desc<"xor", GPRNM16R3Opnd, xor>,
	       ArithLogicR16_Enc<0b01>;
def AND16_NM : ArithLogicR16_Desc<"and", GPRNM16R3Opnd, and>,
	       ArithLogicR16_Enc<0b10>;
def OR16_NM  : ArithLogicR16_Desc<"or	", GPRNM16R3Opnd, or>,
	       ArithLogicR16_Enc<0b11>;
}

class CommuteBinOpAlias<string op, InstNM OpCode, RegisterOperand RO> :
	MipsInstAlias<!strconcat(op, "\t$rd, $rt, $rs"),
		(OpCode RO:$rd, RO:$rs, RO:$rt), 0>,
		ISA_NANOMIPS;

def : CommuteBinOpAlias<"and", AND16_NM, GPRNM16R3Opnd>;
def : CommuteBinOpAlias<"or", OR16_NM, GPRNM16R3Opnd>;
def : CommuteBinOpAlias<"xor", XOR16_NM, GPRNM16R3Opnd>;

class ANDI16_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<3> rt;
  bits<3> rs;
  bits<4> imm;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
  let Inst{3...0} = imm;
}

let FastISelShouldIgnore = 1 in {
def ANDI16_NM : ArithLogicINM<"andi", uimm4mask_nm, GPRNM16R3Opnd,
                               imm4Mask, and>, ANDI16_Enc<0b11100>;
}

// _POOL32A0_0 pool of instructions.
def SLLV_NM  : ArithLogicR32_Desc<"sllv", GPRNM32Opnd, shl>,
	       ArithLogicR32_Enc<0b000010>;
def SRLV_NM  : ArithLogicR32_Desc<"srlv", GPRNM32Opnd, srl>,
	       ArithLogicR32_Enc<0b000110>;
def SRAV_NM  : ArithLogicR32_Desc<"srav", GPRNM32Opnd, sra>,
	       ArithLogicR32_Enc<0b001010>;
def ROTRV_NM : ArithLogicR32_Desc<"rotrv", GPRNM32Opnd, rotr>,
	       ArithLogicR32_Enc<0b001110>;
def ADD_NM   : ArithLogicR32_Desc<"add", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b010010>;
def ADDu_NM  : ArithLogicR32_Desc<"addu", GPRNM32Opnd, add>,
	       ArithLogicR32_Enc<0b010110>;
def SUB_NM   : ArithLogicR32_Desc<"sub", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b011010>;
def SUBu_NM  : ArithLogicR32_Desc<"subu", GPRNM32Opnd, sub>,
	       ArithLogicR32_Enc<0b011110>;
def AND_NM   : ArithLogicR32_Desc<"and", GPRNM32Opnd, and>,
	       ArithLogicR32_Enc<0b100110>;
def OR_NM    : ArithLogicR32_Desc<"or", GPRNM32Opnd, or>,
	       ArithLogicR32_Enc<0b101010>;
def NOR_NM   : ArithLogicR32_Desc<"nor", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b101110>;
def XOR_NM   : ArithLogicR32_Desc<"xor", GPRNM32Opnd, xor>,
	       ArithLogicR32_Enc<0b110010>;
def SLT_NM   : ArithLogicR32_Desc<"slt", GPRNM32Opnd, setlt>,
	       ArithLogicR32_Enc<0b110110>;
def SLTU_NM  : ArithLogicR32_Desc<"sltu", GPRNM32Opnd, setult>,
	       ArithLogicR32_Enc<0b111010>;
def MUL_NM   : ArithLogicR32_Desc<"mul", GPRNM32Opnd, mul>,
	       ArithLogicR32_Enc<0b000011>;
def MUH_NM   : ArithLogicR32_Desc<"muh", GPRNM32Opnd, mulhs>,
	       ArithLogicR32_Enc<0b000111>;
def MULU_NM  : ArithLogicR32_Desc<"mulu", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b001011>;
def MUHU_NM  : ArithLogicR32_Desc<"muhu", GPRNM32Opnd, mulhu>,
	       ArithLogicR32_Enc<0b001111>;
def DIV_NM   : DivMod<"div", sdiv>,
	       ArithLogicR32_Enc<0b010011>;
def MOD_NM   : DivMod<"mod", srem>,
	       ArithLogicR32_Enc<0b010111>;
def DIVU_NM  : DivMod<"divu", udiv>,
	       ArithLogicR32_Enc<0b011011>;
def MODU_NM  : DivMod<"modu", urem>,
	       ArithLogicR32_Enc<0b011111>;
def SOV_NM   : ArithLogicR32_Desc<"sov", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b111110>;

def NOT_NM   : MipsInstAlias<"not $rt, $rs",
			    (NOR_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rs, ZERO_NM)>,
			    ISA_NANOMIPS;

// 16-bit register operations
let isCommutable=1 in {
def SUBu16_NM  : AddSubR16_Desc<"subu", GPRNM16R3Opnd, sub>,
		 AddSubR16_Enc<0b1>;
def ADDu16_NM : AddSubR16_Desc<"addu", GPRNM16R3Opnd, add>,
		AddSubR16_Enc<0b0>;
def ADDu4x4_NM : Arith4x4_Desc<"addu", GPRNM4Opnd, add>,
		 Arith4x4_Enc<0b00>, NoEncode<"$rt">;
def MUL4x4_NM  : Arith4x4_Desc<"mul", GPRNM4Opnd, mul>,
		  Arith4x4_Enc<0b01>, NoEncode<"$rt">;
}

def : CommuteBinOpAlias<"addu", ADDu4x4_NM, GPRNM4Opnd>;
def : CommuteBinOpAlias<"mul", MUL4x4_NM, GPRNM4Opnd>;


// P.U12 pool of instructions
def ORI_NM   : CondLogicI32_Desc<"ori", uimm12_nm, GPRNM32Opnd, imm32ZExt12, or>,
	       CondLogicI32_Enc<0b0000>;
def XORI_NM  : CondLogicI32_Desc<"xori", uimm12_nm, GPRNM32Opnd, imm32ZExt12, xor>,
	       CondLogicI32_Enc<0b0001>;
def ANDI_NM  : CondLogicI32_Desc<"andi", uimm12_nm, GPRNM32Opnd, imm32ZExt12ANDI, and>,
	       CondLogicI32_Enc<0b0010>;
def SLTI_NM  : CondLogicI32_Desc<"slti", uimm12_nm, GPRNM32Opnd, imm32ZExt12, setlt>,
	       CondLogicI32_Enc<0b0100>;
def SLTIU_NM : CondLogicI32_Desc<"sltiu", uimm12_nm, GPRNM32Opnd, imm32ZExt12, setult>,
	       CondLogicI32_Enc<0b0101>;
def SEQI_NM  : CondLogicI32_Desc<"seqi", uimm12_nm, GPRNM32Opnd, imm32ZExt12, seteq>,
	       CondLogicI32_Enc<0b0110>;

// P.SHIFT pool of instructions
def SLL16_NM  : ShiftI_Desc<"sll", uimm3shift_nm, GPRNM16R3Opnd, imm32ZExt3, shl>,
	      ShiftI16_Enc<0b0>;
def SLL_NM  : ShiftI_Desc<"sll", uimm5, GPRNM32Opnd, imm32ZExt5, shl>,
	      ShiftI32_Enc<0b0000>;
def SRL16_NM  : ShiftI_Desc<"srl", uimm3shift_nm, GPRNM16R3Opnd, imm32ZExt3, srl>,
	      ShiftI16_Enc<0b1>;
def SRL_NM  : ShiftI_Desc<"srl", uimm5, GPRNM32Opnd, imm32ZExt5, srl>,
	      ShiftI32_Enc<0b0010>;
def SRA_NM  : ShiftI_Desc<"sra", uimm5, GPRNM32Opnd, imm32ZExt5, sra>,
	      ShiftI32_Enc<0b0100>;
def ROTR_NM : ShiftI_Desc<"rotr", uimm5, GPRNM32Opnd, imm32ZExt5, rotr>,
	      ShiftI32_Enc<0b0110>;

def EXT_NM : ExtBaseNM, ExtInsBase_Enc<0b1111>;
def INS_NM : InsBaseNM, ExtInsBase_Enc<0b1110>;

def ADDIU_NM : AddImmediate<"addiu", uimm16_nm, GPRNM32NZOpnd, GPRNM32Opnd,
                            imm32ZExt16, add>, _Pool_P32<0b00000> {
  bits<5> rt;
  bits<5> rs;
  bits<16> imm;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...0} = imm;
  let TwoOperandAliasConstraint = "$rt = $rs";
}

def LI_NM : MipsInstAlias<"li $rt, $imm",
			  (ADDIU_NM GPRNM32NZOpnd:$rt, ZERO_NM, uimm16_nm:$imm)>,
			  ISA_NANOMIPS;

def ADDIUNEG_NM : AddImmediate<"addiu", nimm12_nm, GPRNM32Opnd, GPRNM32Opnd,
                               imm32Neg12, add>, CondLogicI32_Enc<0b1000>;

def LINEG_NM : MipsInstAlias<"li $rt, $imm",
			     (ADDIUNEG_NM GPRNM32Opnd:$rt, ZERO_NM, nimm12_nm:$imm)>,
			     ISA_NANOMIPS;

def ADDIUR2_NM : AddImmediate<"addiu", uimm5s2_nm, GPRNM16R3Opnd, GPRNM16R3Opnd,
                              imm32ZExt5, add>, _Pool_P16_A2<0b0> {
  bits<3> rt;
  bits<3> rs;
  bits<5> imm;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
  let Inst{2...0} = imm{4...2};
}

def ADDIURS5_NM : AddImmediate<"addiu", simm4, GPRNM16R5NZOpnd, GPRNM16R5NZOpnd,
			       imm32ZExt4, add>, _Pool_P16_A2<0b1>,
			       RegConstraint<"$rs = $rt">, NoEncode<"$rs">  {
  bits<5> rt;
  bits<4> imm;
  let Inst{9...5} = rt;
  let Inst{4} = imm{3};
  let Inst{2...0} = imm{2...0};
}

def ADDIUR1SP_NM : AddImmediate<"addiu", uimm8s2_nm, GPRNM16R3Opnd,
				GPRNMSPOpnd, imm32ZExt8s2, add>,
				_Pool_P16<0b01100>,
				NoEncode<"$rs"> {
  bits<3> rt;
  bits<8> imm;
  let Inst{9...7} = rt;
  let Inst{6} = 0b1;
  let Inst{5...0} = imm{7...2};
}

class Op48_Imm_Enc<bits<5> isel> : _Pool_P48I<isel> {
  bits<5> rt;
  bits<32> imm;
  let Inst{41...37} = rt;
  let Inst{31...16} = imm{15...0};
  let Inst{15...0} = imm{31...16};
}

def ADDIU48_NM : ArithLogicINM<"addiu", sym32_nm, GPRNM48Opnd,
			imm32_NM, add>,
			Op48_Imm_Enc<0b00001>,
			RegConstraint<"$rs = $rt">;

class AddImmediateGP<string opstr, Operand Od, RegisterOperand RO,
		     ComplexPattern offset_type,
		     SDPatternOperator OpNode = null_frag> :
  InstNM<(outs RO:$rt), (ins GPRNMGPOpnd:$rs, Od:$offset),
	 !strconcat(opstr, "\t$rt, $rs, $offset"),
	 [(set RO:$rt, (add GPRNMGPOpnd:$rs, offset_type:$offset))]>;

def ADDIUGPB_NM : AddImmediateGP<"addiu", sym32_gp18_nm, GPRNM32Opnd,
				offsetgp18, add>,
				_Pool_P_GP_BH<0b011>, NoEncode<"$rs"> {
  bits<5> rt;
  bits<18> offset;
  let Inst{25...21} = rt;
  let Inst{17...0} = offset;
 }

def ADDIUGPW_NM : AddImmediateGP<"addiu[gp.w]", sym32_gp19s2_nm, GPRNM32Opnd,
				offsetgp19s2>,
				_Pool_P_GP_W<0b00>, NoEncode<"$rs"> {
  bits<5> rt;
  bits<21> offset;
  let Inst{25...21} = rt;
  let Inst{20...2} = offset{20...2};
}

class Op32_Addr_Enc<bits<5> isel> : _Pool_P32<isel> {
  bits<5> rt;
  bits<22> addr;
  let Inst{25...21} = rt;
  let Inst{20...1} = addr{20...1};
  let Inst{0} = addr{21};
}

def : MipsInstAlias<"addiu.b $rt, $rs, $offset",
			(ADDIUGPB_NM GPRNM32Opnd:$rt, GPRNMGPOpnd:$rs, sym32_gp_nm:$offset)>,
			ISA_NANOMIPS;
def : MipsInstAlias<"addiu.w $rt, $rs, $offset",
			(ADDIUGPW_NM GPRNM32Opnd:$rt, GPRNMGPOpnd:$rs, sym32_gp_nm:$offset)>,
			ISA_NANOMIPS;

let isReMaterializable = 1 in {
  def LAPC32_NM : LoadAddressPCRelNM<"lapc.h", GPRNM32Opnd, sym32_pc_nm,
				     addrsym>, Op32_Addr_Enc<0b00001>;
  def LAPC48_NM : LoadAddressPCRelNM<"lapc.b", GPRNM48Opnd, sym32_pc_nm,
				     addrsym>, Op48_Addr_Enc<0b00011>;
}

def ADDIUPC48_NM : MipsInstAlias<"addiupc $rt, $addr",
			    (LAPC48_NM GPRNM48Opnd: $rt, sym32_pc_nm:$addr), 0>,
			    ISA_NANOMIPS;
def : MnemonicAlias<"lapc", "lapc.h">, ISA_NANOMIPS;

def LSA_NM : LoadScaledAddressNM<"lsa", GPRNM32Opnd>, LoadScaledAddress_Enc;

def SEB_NM : SignExtInRegNM<"seb", i8, GPRNM32Opnd>, LogicU32_Enc<0b000001>;
def SEH_NM : SignExtInRegNM<"seh", i16, GPRNM32Opnd>, LogicU32_Enc<0b000101>;

def CLZ_NM : CountLeading0NM<"clz", GPRNM32Opnd>, CLZO_Enc<0b0101101>;
def CLO_NM : CountLeading1NM<"clo", GPRNM32Opnd>, CLZO_Enc<0b0100101>;

// P.TRAP pool of instructions.
def TEQ_NM : Trap<"teq">;
def TNE_NM : Trap<"tne">;

def JRC_NM : IndirectBranchNM<"jrc", GPRNM32Opnd>;

let isCodeGenOnly = 1, hasNoSchedulingInfo = 1,
    hasDelaySlot = 0, isBranch = 1,
    isBarrier = 1,
   isTerminator = 1,
   hasDelaySlot = 0,
   isCTI = 1 in
def BRSC_NM : InstNM<(outs), (ins GPRNM32Opnd:$rs, i32imm:$jti), "brsc\t$rs", [(NMbr_jt GPRNM32Opnd:$rs, tjumptable:$jti)]>, InstSize32;

def BALC_NM : CallNM<"balc", MipsJmpLink, tglobaladdr>, InstSize32;
def MOVEBALC_NM : MoveBalcBase;

class LI16_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<3> rt;
  bits<7> eu;
  let Inst{9...7} = rt;
  let Inst{6...0} = eu;
}

let isReMaterializable = 1 in {
  def LI16_NM : LoadImmediateNM<"li[16]", GPRNM16R3Opnd, nimm7_nm, imm7M1To126>,
	        LI16_Enc<0b11000>;

  def LI48_NM : LoadAddressAbsNM<"li[48]", GPRNM48Opnd, sym32_nm,
			         addrsym>, Op48_Imm_Enc<0b00000>;

  def ADDIUGP48_NM : LoadAddressGPRelNM<"addiu[gp48]", GPRNM48Opnd, sym32_gp_nm,
				        addrgprel>, Op48_Addr_Enc<0b00010>,
				        NoEncode<"$rs">;
}

def ADDIUB32_NM : MipsInstAlias<"addiu.b32 $rt, $rs, $addr",
				(ADDIUGP48_NM GPRNM48Opnd:$rt,
				GPRNMGPOpnd:$rs, sym32_gp_nm:$addr)>,
				ISA_NANOMIPS;
def LA_NM : MipsInstAlias<"la $rt, $addr",
			  (LAPC48_NM GPRNM48Opnd:$rt, sym32_pc_nm:$addr)>,
			  ISA_NANOMIPS;

class LUI_Enc<bits<1> isel> : _Pool_P_LUI<isel> {
  bits<5> rt;
  bits<20> imm;
  let Inst{25...21} = rt;
  let Inst{20...12} = imm{8...0};
  let Inst{11...2} = imm{18...9};
  let Inst{0} = imm{19};
}

let isReMaterializable = 1 in {
  def LUI_NM : RegImmNM<"lui", simm32s12_nm, GPRNM32Opnd, imm32SExt20s12>,
	       LUI_Enc<0b0>;
}

def : NMPat<(i32 imm32SExt20s12:$imm), (LUI_NM imm32SExt20s12:$imm)>;

def ALUIPC_NM : RegImmNM<"aluipc", simm32s12_pcrel_nm, GPRNM32Opnd, imm32SExt20s12>, LUI_Enc<0b1>;

def LEA_ADDIU_NM : EffectiveAddressNM<"addiu", GPRNM32Opnd>, _Pool_P32<0b00000> {
  bits<5> rt;
  bits<16> addr;
  let Inst{25...21} = rt;
  let Inst{20...16} = 0b00000;
  let Inst{15...0} = addr;
}

def RDHWR_NM : ReadHardwareNM<GPRNM32Opnd, HWRegsOpnd>;

def ADJCALLSTACKDOWN_NM : AdjustStack<callseq_start>;
def ADJCALLSTACKUP_NM   : AdjustStack<callseq_end>;

// Return instruction is matched as RetRA, then expanded into PseudoReturnNM
// after register allocation. Finally, MipsAsmPrinter expands this into JRC_NM.
def PseudoReturnNM : PseudoInstNM<(outs), (ins GPRNM32Opnd:$rs), []> {
  let isTerminator = 1;
  let isBarrier = 1;
  let isReturn = 1;
  let isCodeGenOnly = 1;
  let hasCtrlDep = 1;
  let hasExtraSrcRegAllocReq = 1;
  bit isCTI = 1;
}

let isCodeGenOnly = 1, hasNoSchedulingInfo = 1, hasDelaySlot = 0 in
def LoadJumpTableOffset : PseudoInstNM<(outs GPRNM32Opnd:$rd), (ins GPRNM32Opnd:$table, GPRNM32Opnd:$entry, i32imm:$jti), []>;

// Indirect branch is matched as PseudoIndirectBranchNM and expanded to JRC_NM.
def PseudoIndirectBranchNM :
    PseudoInstNM<(outs), (ins GPRNM32Opnd:$rs), [(brind GPRNM32Opnd:$rs)]>,
    PseudoInstExpansion<(JRC_NM GPRNM32Opnd:$rs)> {
  let isTerminator = 1;
  let isBarrier = 1;
  let isBranch = 1;
  let isIndirectBranch = 1;
  bit isCTI = 1;
}

let hasPostISelHook = 1, isCall = 1, isCTI = 1, Defs = [RA_NM] in {
  def JALRC_NM : InstNM<(outs GPRNM32Opnd:$rd), (ins GPRNM32Opnd:$rs),
                        "jalrc\t$rd, $rs", []>, InstSize32;

  def JALRCPseudo : PseudoInstNM<(outs), (ins GPRNM32Opnd:$rs),
                                 [(MipsJmpLink GPRNM32Opnd:$rs)]>,
    PseudoInstExpansion<(JALRC_NM RA_NM, GPRNM32Opnd:$rs)>;
}

def : NMPat<(i32 imm32_NM:$imm), (LI48_NM imm:$imm)>;

def : NMPat<(MipsJmpLink (i32 texternalsym:$dst)), (BALC_NM texternalsym:$dst)>;

def : NMPat<(MipsFullAddr tglobaladdr:$in), (LAPC48_NM tglobaladdr:$in)>;
def : NMPat<(MipsFullAddr tglobaltlsaddr:$in), (LAPC48_NM tglobaltlsaddr:$in)>;
def : NMPat<(MipsFullAddr tblockaddress:$in), (LAPC48_NM tblockaddress:$in)>;
def : NMPat<(MipsFullAddr tjumptable:$in), (LAPC48_NM tjumptable:$in)>;
def : NMPat<(MipsFullAddrAdd GPRNM32:$rs, tglobaladdr:$in), (ADDIU48_NM GPRNM32:$rs, tglobaladdr:$in)>;
 
def : NMPat<(not (or GPRNM32:$rs, GPRNM32:$rt)),
            (NOR_NM GPRNM32:$rs, GPRNM32:$rt)>;

def : NMPat<(trap), (TEQ_NM ZERO_NM, ZERO_NM, 0)>;

// SetCC patterns.
def : NMPat<(setne GPRNM32:$lhs, 0), (SLTU_NM ZERO_NM, GPRNM32:$lhs)>;
def : NMPat<(seteq GPRNM32:$lhs, GPRNM32:$rhs),
            (SLTIU_NM (XOR_NM GPRNM32:$lhs, GPRNM32:$rhs), 1)>;
def : NMPat<(setne GPRNM32:$lhs, GPRNM32:$rhs),
            (SLTU_NM ZERO_NM, (XOR_NM GPRNM32:$lhs, GPRNM32:$rhs))>;
def : NMPat<(setle GPRNM32:$lhs, GPRNM32:$rhs),
            (XORI_NM (SLT_NM GPRNM32:$rhs, GPRNM32:$lhs), 1)>;
def : NMPat<(setule GPRNM32:$lhs, GPRNM32:$rhs),
            (XORI_NM (SLTU_NM GPRNM32:$rhs, GPRNM32:$lhs), 1)>;
def : NMPat<(setgt GPRNM32:$lhs, GPRNM32:$rhs),
            (SLT_NM GPRNM32:$rhs, GPRNM32:$lhs)>;
def : NMPat<(setugt GPRNM32:$lhs, GPRNM32:$rhs),
            (SLTU_NM GPRNM32:$rhs, GPRNM32:$lhs)>;
def : NMPat<(setge GPRNM32:$lhs, GPRNM32:$rhs),
            (XORI_NM (SLT_NM GPRNM32:$lhs, GPRNM32:$rhs), 1)>;
def : NMPat<(setuge GPRNM32:$lhs, GPRNM32:$rhs),
            (XORI_NM (SLTU_NM GPRNM32:$lhs, GPRNM32:$rhs), 1)>;

//===----------------------------------------------------------------------===//
//
// Jump and Branch Instructions.
//
//===----------------------------------------------------------------------===//

// NanoMips has only 'compact' jumps and branches, with no delay slots.
// Described using IsntSE class for the moment, should use specific
// NanoMips instruction formats later to get correct sizes.  Some
// branch instructions exist in multiple different sizes, and should
// likely be distinct instructions later.

// This class is missing instruction size, because these instructions can be
// encoded in formats of different size (eg. BEQC16 and BEQC32).
class CBranchNM<string opstr, DAGOperand opnd, PatFrag cond_op,
                RegisterOperand RO>
    : InstNM<(outs), (ins RO:$rs, RO:$rt, opnd:$offset),
             !strconcat(opstr, "\t$rs, $rt, $offset"),
             [(brcond (i32 (cond_op RO:$rs, RO:$rt)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  let hasDelaySlot = 0;
  bit isCTI = 1;
}

class CBranchImmNM<string opstr, DAGOperand opnd, PatFrag cond_op,
                   RegisterOperand RO,
                   Operand imm_type, SDPatternOperator ImmOp>
    : InstSize32,
      InstNM<(outs), (ins RO:$rt, imm_type:$u, opnd:$offset),
             !strconcat(opstr, "\t$rt, $u, $offset"),
             [(brcond (i32 (cond_op RO:$rt, ImmOp:$u)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  let hasDelaySlot = 0;
  bit isCTI = 1;
}

class CBranchZeroNM<string opstr, DAGOperand opnd, PatFrag cond_op,
                    RegisterOperand RO>
    : InstSize16,
      InstNM<(outs), (ins RO:$rs, opnd:$offset),
             !strconcat(opstr, "\t$rs, $offset"),
             [(brcond (i32 (cond_op RO:$rs, 0)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  bit isCTI = 1;
}

class CBranchBitNM<string opstr, DAGOperand opnd, PatFrag cond_op,
                   RegisterOperand RO,
                   Operand imm_type, SDPatternOperator ImmOp>
    : InstSize32,
      InstNM<(outs), (ins RO:$rt, imm_type:$u, opnd:$offset),
             !strconcat(opstr, "\t$rt, $u, $offset"),
        [(brcond (i32 (cond_op (and RO:$rt, (shl 1, ImmOp:$u)), 0)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  let hasDelaySlot = 0;
  bit isCTI = 1;
}


let isCodeGenOnly = 1, hasNoSchedulingInfo = 1,
    hasDelaySlot = 0 in {

// TODO: BBEQZ/BBNEZ bit-test conditional branch instructions
def BEQC_NM  : CBranchNM<"beqc", brtarget, seteq, GPRNM32Opnd>, InstSize32;
def BEQIC_NM : CBranchImmNM<"beqic", brtarget, seteq, GPRNM32Opnd, uimm7, immZExt7>;
def BEQZC_NM : CBranchZeroNM<"beqzc", brtarget, seteq, GPRNM32Opnd>;

def BGEC_NM   : CBranchNM<"bgec", brtarget, setge, GPRNM32Opnd>, InstSize32;
def BGEIC_NM  : CBranchImmNM<"bgeic", brtarget, setge, GPRNM32Opnd, uimm7, immZExt7>;
def BGEIUC_NM : CBranchImmNM<"bgeiuc", brtarget, setuge, GPRNM32Opnd, uimm7, immZExt7>;
def BGEUC_NM  : CBranchNM<"bgeuc", brtarget, setuge, GPRNM32Opnd>, InstSize32;

def BLTC_NM   : CBranchNM<"bltc", brtarget, setlt, GPRNM32Opnd>, InstSize32;
def BLTIC_NM  : CBranchImmNM<"bltic", brtarget, setlt, GPRNM32Opnd, uimm7, immZExt7>;
def BLTIUC_NM : CBranchImmNM<"bltiuc", brtarget, setult, GPRNM32Opnd, uimm7, immZExt7>;
def BLTUC_NM  : CBranchNM<"bltuc", brtarget, setult, GPRNM32Opnd>, InstSize32;

def BNEC_NM  : CBranchNM<"bnec", brtarget, setne, GPRNM32Opnd>, InstSize32;
def BNEIC_NM : CBranchImmNM<"bneic", brtarget, setne, GPRNM32Opnd, uimm7, immZExt7>;
def BNEZC_NM : CBranchZeroNM<"bnezc", brtarget, setne, GPRNM32Opnd>;

// Bit-test branches
def BBNEZC_NM : CBranchBitNM<"bbnezc", brtarget, setne, GPRNM32Opnd, simm32, simm32power2>;
def BBEQZC_NM : CBranchBitNM<"bbeqzc", brtarget, seteq, GPRNM32Opnd, simm32, simm32power2>;

}

class BCBase : InstNM<(outs), (ins jmptarget:$target), "bc\t$target",
                      [(br bb:$target)], II_J>, InstSize32 {
  let isTerminator = 1;
  let isBarrier = 1;
  let isBranch = 1;
  let isCTI = 1;
}
def BC_NM : BCBase;

class TailCallBase : PseudoInstNM<(outs), (ins calltarget:$target), []>,
                     PseudoInstExpansion<(BC_NM jmptarget:$target)> {
  let isCall = 1;
  let isTerminator = 1;
  let isReturn = 1;
  let isBarrier = 1;
  let isCTI = 1;
}
def TAILCALL_NM : TailCallBase;

class TailCallRegBase : PseudoInstNM<(outs), (ins RegisterOperand<GPRNM32_TAIL>:$rs),
                                     [(MipsTailCall RegisterOperand<GPRNM32_TAIL>:$rs)]>,
                        PseudoInstExpansion<(JRC_NM GPRNM32Opnd:$rs)> {
  let isCall = 1;
  let isTerminator = 1;
  let isReturn = 1;
  let isBarrier = 1;
  let isCTI = 1;
  let hasPostISelHook = 1;
}
def TAILCALLREG_NM : TailCallRegBase;

def : NMPat<(MipsTailCall (i32 tglobaladdr:$dst)),
            (TAILCALL_NM tglobaladdr:$dst)>;
def : NMPat<(MipsTailCall (i32 texternalsym:$dst)),
            (TAILCALL_NM texternalsym:$dst)>;

// '>-1' can be represented as '>=0'.
def : NMPat<(brcond (i32 (setgt GPRNM32:$lhs, -1)), bb:$dst),
            (BGEC_NM GPRNM32:$lhs, ZERO_NM, bb:$dst)>;

// Reversed branch conditions:
def : NMPat<(brcond (i32 (setugt GPRNM32:$lhs, GPRNM32:$rhs)), bb:$dst),
            (BLTUC_NM GPRNM32:$rhs, GPRNM32:$lhs, bb:$dst)>;
def : NMPat<(brcond (i32 (setgt GPRNM32:$lhs, GPRNM32:$rhs)), bb:$dst),
            (BLTC_NM GPRNM32:$rhs, GPRNM32:$lhs, bb:$dst)>;
def : NMPat<(brcond (i32 (setule GPRNM32:$lhs, GPRNM32:$rhs)), bb:$dst),
            (BGEUC_NM GPRNM32:$rhs, GPRNM32:$lhs, bb:$dst)>;
def : NMPat<(brcond (i32 (setle GPRNM32:$lhs, GPRNM32:$rhs)), bb:$dst),
            (BGEC_NM GPRNM32:$rhs, GPRNM32:$lhs, bb:$dst)>;

// Immediate comparison branches with offset
def : NMPat<(brcond (i32 (setgt GPRNM32:$lhs, immZExt7Plus1:$imm)), bb:$dst),
            (BGEIC_NM GPRNM32:$lhs, (Plus1 $imm), bb:$dst)>;
def : NMPat<(brcond (i32 (setugt GPRNM32:$lhs, immZExt7Plus1:$imm)), bb:$dst),
            (BGEIUC_NM GPRNM32:$lhs, (Plus1 $imm), bb:$dst)>;

// Branch on integer value
def : NMPat<(brcond (i32 GPRNM32:$lhs), bb:$dst),
            (BNEC_NM GPRNM32:$lhs, ZERO_NM, bb:$dst)>;

// Bit-test branch
def : NMPat<(brcond (i32 (setne (and GPRNM32:$rt, simm32power2:$imm), 0)), bb:$offset),
          (BBNEZC_NM GPRNM32:$rt, (Log2XForm $imm), bb:$offset)>;
def : NMPat<(brcond (i32 (seteq (and GPRNM32:$rt, simm32power2:$imm), 0)), bb:$offset),
          (BBEQZC_NM GPRNM32:$rt, (Log2XForm $imm), bb:$offset)>;


//===----------------------------------------------------------------------===//
//
// Load / store instructions
//
//===----------------------------------------------------------------------===//

let isCodeGenOnly = 1 in {

// TODO: addruimm12 and addrsimm9 need to be looked into again. Currently
//       addruimm12 needs to come before addrsimm9, because addrsimm9 handles
//       loads/store that do not fit the offset. If we put the same handling in
//       addruimm12, this will cause addrsimm9 to never get selected.
def LWs9_NM : LoadMemoryNM<"lw", GPRNM32Opnd, mem, load, addrsimm9, II_LW>, InstSize32;
def LW_NM : LoadMemoryNM<"lw", GPRNM32Opnd, mem, load, addruimm12, II_LW>, InstSize32;
def LWXS_NM : LoadMemoryNM<"lwxs", GPRNM32Opnd, mem, load, addrindexedlsl2, II_LW>, InstSize32;
def LWX_NM : LoadMemoryNM<"lwx", GPRNM32Opnd, mem, load, addrindexed, II_LW>, InstSize32;
def SW_NM : StoreMemoryNM<"sw", GPRNM32Opnd, mem, store, addruimm12, II_SW>, InstSize32;
def SWs9_NM : StoreMemoryNM<"sw", GPRNM32Opnd, mem, store, addrsimm9, II_SW>, InstSize32;
def SWXS_NM : StoreMemoryNM<"swxs", GPRNM32Opnd, mem, store, addrindexedlsl2, II_SW>, InstSize32;
def SWX_NM : StoreMemoryNM<"swx", GPRNM32Opnd, mem, store, addrindexed, II_SW>, InstSize32;

def LHU_NM : LoadMemoryNM<"lhu", GPRNM32Opnd, mem, zextloadi16, addruimm12, II_LHU>, InstSize32;
def LHUs9_NM : LoadMemoryNM<"lhu", GPRNM32Opnd, mem, zextloadi16, addrsimm9, II_LHU>, InstSize32;
def LHUXS_NM : LoadMemoryNM<"lhuxs", GPRNM32Opnd, mem, zextloadi16, addrindexedlsl1, II_LHU>, InstSize32;
def LHUX_NM : LoadMemoryNM<"lhux", GPRNM32Opnd, mem, zextloadi16, addrindexed, II_LHU>, InstSize32;
def LH_NM : LoadMemoryNM<"lh", GPRNM32Opnd, mem, sextloadi16, addruimm12, II_LH>, InstSize32;
def LHs9_NM : LoadMemoryNM<"lh", GPRNM32Opnd, mem, sextloadi16, addrsimm9, II_LH>, InstSize32;
def LHXS_NM : LoadMemoryNM<"lhxs", GPRNM32Opnd, mem, sextloadi16, addrindexedlsl1, II_LH>, InstSize32;
def LHX_NM : LoadMemoryNM<"lhx", GPRNM32Opnd, mem, sextloadi16, addrindexed, II_LH>, InstSize32;
def SH_NM : StoreMemoryNM<"sh", GPRNM32Opnd, mem, truncstorei16, addruimm12, II_SH>, InstSize32;
def SHs9_NM : StoreMemoryNM<"sh", GPRNM32Opnd, mem, truncstorei16, addrsimm9, II_SH>, InstSize32;
def SHXS_NM : StoreMemoryNM<"shxs", GPRNM32Opnd, mem, truncstorei16, addrindexedlsl1, II_SH>, InstSize32;
def SHX_NM : StoreMemoryNM<"shx", GPRNM32Opnd, mem, truncstorei16, addrindexed, II_SH>, InstSize32;

def LBU_NM : LoadMemoryNM<"lbu", GPRNM32Opnd, mem, zextloadi8, addruimm12, II_LBU>, InstSize32;
def LBUs9_NM : LoadMemoryNM<"lbu", GPRNM32Opnd, mem, zextloadi8, addrsimm9, II_LBU>, InstSize32;
def LBUX_NM : LoadMemoryNM<"lbux", GPRNM32Opnd, mem, zextloadi8, addrindexed, II_LBU>, InstSize32;
def LB_NM : LoadMemoryNM<"lb", GPRNM32Opnd, mem, sextloadi8, addruimm12, II_LB>, InstSize32;
def LBs9_NM : LoadMemoryNM<"lb", GPRNM32Opnd, mem, sextloadi8, addrsimm9, II_LB>, InstSize32;
def LBX_NM : LoadMemoryNM<"lbx", GPRNM32Opnd, mem, sextloadi8, addrindexed, II_LB>, InstSize32;
def SB_NM : StoreMemoryNM<"sb", GPRNM32Opnd, mem, truncstorei8, addruimm12, II_SB>, InstSize32;
def SBs9_NM : StoreMemoryNM<"sb", GPRNM32Opnd, mem, truncstorei8, addrsimm9, II_SB>, InstSize32;
def SBX_NM : StoreMemoryNM<"sbx", GPRNM32Opnd, mem, truncstorei8, addrindexed, II_SB>, InstSize32;

// TODO: Halfword load/store is never selected, this needs to be looked into.
def UALH_NM : UnalignedLoad<"ualh", NMUnalignedLH>;
def UALW_NM : UnalignedLoad<"ualw", NMUnalignedLW>;
def UASH_NM : UnalignedStore<"uash", NMUnalignedSH>;
def UASW_NM : UnalignedStore<"uasw", NMUnalignedSW>;

def LWGP_NM : LoadMemoryNM<"lw", GPRNM32Opnd, mem>, InstSize32;

def SWPC_NM : StorePCBase<GPRNM32Opnd, simm32>;
def LWPC_NM : LoadPCBase<GPRNM32Opnd, simm32>;

def LWM_NM : LoadMultipleNM<"lwm">;
def UALWM_NM : LoadMultipleNM<"ualwm">;
def SWM_NM : StoreMultipleNM<"swm">;
def UASWM_NM : StoreMultipleNM<"uaswm">;

def MOVE_NM : InstNM<(outs GPRNM32Opnd:$rt), (ins GPRNM32Opnd:$rs),
                      "move\t$rt, $rs", []>, InstSize16;
def MOVEP_NM : InstNM<(outs GPRNM32Opnd:$dst1, GPRNM32Opnd:$dst2),
                      (ins GPRNM32Opnd:$src1, GPRNM32Opnd:$src2),
                      "movep\t$dst1, $dst2, $src1, $src2", []>, InstSize16;

}

// These patterns help generate better loads/stores when the immediate doesn't
// fit and ends up being and indexed load/store. For example:
//
//  li $a0, -300        ->  addiu $a0, $a2, -300
//  lbux $a1, $a0($a2)  ->  lbu $a1, 0($a0)
//
// On the left is what would be generated without these patterns. This results
// in two 32-bit instructions. While on the right, we have one 32-bit and one
// 16-bit instruction. It does require the right register, in order for second
// instruction to be 16-bit, but it's still better than always having two 32-bit
// instructions.
//
let AddedComplexity = 3 in {
def : NMPat<(i32 (extloadi8 (add GPRNM32:$rs, immFitsAddiu32:$imm))),
            (LBU_NM (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
def : NMPat<(i32 (zextloadi8 (add GPRNM32:$rs, immFitsAddiu32:$imm))),
            (LBU_NM (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
def : NMPat<(i32 (sextloadi8 (add GPRNM32:$rs, immFitsAddiu32:$imm))),
            (LB_NM (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
def : NMPat<(i32 (extloadi16 (add GPRNM32:$rs, immFitsAddiu32:$imm))),
            (LHU_NM (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
def : NMPat<(i32 (zextloadi16 (add GPRNM32:$rs, immFitsAddiu32:$imm))),
            (LHU_NM (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
def : NMPat<(i32 (sextloadi16 (add GPRNM32:$rs, immFitsAddiu32:$imm))),
            (LH_NM (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
def : NMPat<(i32 (load (add GPRNM32:$rs, immFitsAddiu32:$imm))),
            (LW_NM (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
def : NMPat<(truncstorei8 GPRNM32:$rt, (add GPRNM32:$rs, immFitsAddiu32:$imm)),
            (SB_NM GPRNM32:$rt, (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
def : NMPat<(truncstorei16 GPRNM32:$rt, (add GPRNM32:$rs, immFitsAddiu32:$imm)),
            (SH_NM GPRNM32:$rt, (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
def : NMPat<(store GPRNM32:$rt, (add GPRNM32:$rs, immFitsAddiu32:$imm)),
            (SW_NM GPRNM32:$rt, (ADDIU_NM GPRNM32:$rs, imm:$imm), 0)>;
} // AddedComplexity = 3

// Catching out-of-range immediate loads/stores.
def : NMPat<(i32 (extloadi8 GPRNM32:$rs)), (LBU_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (zextloadi8 GPRNM32:$rs)), (LBU_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (sextloadi8 GPRNM32:$rs)), (LB_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (extloadi16 GPRNM32:$rs)), (LHU_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (zextloadi16 GPRNM32:$rs)), (LHU_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (sextloadi16 GPRNM32:$rs)), (LH_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (load GPRNM32:$rs)), (LW_NM GPRNM32:$rs, 0)>;
def : NMPat<(truncstorei8 GPRNM32:$rt, GPRNM32:$rs),
            (SB_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;
def : NMPat<(truncstorei16 GPRNM32:$rt, GPRNM32:$rs),
            (SH_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;
def : NMPat<(store GPRNM32:$rt, GPRNM32:$rs),
            (SW_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;

// Any-extending loads
def : NMPat<(i32 (extloadi8 addruimm12:$addr)), (LBU_NM addruimm12:$addr)>;
def : NMPat<(i32 (extloadi8 addrsimm9:$addr)), (LBUs9_NM addrsimm9:$addr)>;
def : NMPat<(i32 (extloadi8 addrindexed:$addr)), (LBUX_NM addrindexed:$addr)>;
def : NMPat<(i32 (extloadi16 addruimm12:$addr)), (LHU_NM addruimm12:$addr)>;
def : NMPat<(i32 (extloadi16 addrsimm9:$addr)), (LHUs9_NM addrsimm9:$addr)>;
def : NMPat<(i32 (extloadi16 addrindexedlsl1:$addr)),
            (LHUXS_NM addrindexedlsl1:$addr)>;
def : NMPat<(i32 (extloadi16 addrindexed:$addr)), (LHUX_NM addrindexed:$addr)>;

// Catching out-of-range immediate unaligned loads/stores.
def : NMPat<(i32 (NMUnalignedLW GPRNM32:$rs, GPRNM32:$src)),
            (UALW_NM GPRNM32:$rs, 0, GPRNM32:$src)>;
def : NMPat<(NMUnalignedSW GPRNM32:$rt, GPRNM32:$rs),
            (UASW_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;
def : NMPat<(i32 (NMUnalignedLH GPRNM32:$rs, GPRNM32:$src)),
            (UALH_NM GPRNM32:$rs, 0, GPRNM32:$src)>;
def : NMPat<(NMUnalignedSH GPRNM32:$rt, GPRNM32:$rs),
            (UASH_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;

// GP-relative load.
def : NMPat<(i32 (load (add GP_NM, (MipsGPRel tglobaladdr:$in)))),
            (LWGP_NM GP_NM, tglobaladdr:$in)>;
def : NMPat<(add GP_NM, (MipsGPRel tglobaladdr:$in)),
            (ADDIU_NM GP_NM, tglobaladdr:$in)>;

// Atomic load patterns.
def : NMPat<(atomic_load_8 addr:$a), (LB_NM addr:$a)>;
def : NMPat<(atomic_load_16 addr:$a), (LH_NM addr:$a)>;
def : NMPat<(atomic_load_32 addr:$a), (LW_NM addr:$a)>;

// Atomic store patterns.
def : NMPat<(atomic_store_8 addr:$a, GPR32:$v), (SB_NM GPR32:$v, addr:$a)>;
def : NMPat<(atomic_store_16 addr:$a, GPR32:$v), (SH_NM GPR32:$v, addr:$a)>;
def : NMPat<(atomic_store_32 addr:$a, GPR32:$v), (SW_NM GPR32:$v, addr:$a)>;

def LL_NM : InstNM<(outs GPRNM32Opnd:$rd), (ins mem_simm9:$addr),
                    "ll\t$rd, $addr", []>, InstSize32 {
  let mayLoad = 1;
  let hasNoSchedulingInfo = 1;
}

def SC_NM : InstNM<(outs GPRNM32Opnd:$dst), (ins GPRNM32Opnd:$rt, mem_simm9:$addr),
                   "sc\t$rt, $addr", []>, InstSize32 {
  let mayStore = 1;
  let Constraints = "$rt = $dst";
  let hasNoSchedulingInfo = 1;
}

// Conditional Moves
def MOVZ_NM : InstNM<(outs GPRNM32Opnd:$rd),
                     (ins GPRNM32Opnd:$rs, GPRNM32Opnd:$rt, GPRNM32Opnd:$F),
                     "movz\t$rd, $rs, $rt",
                     []>, InstSize32 {
  let Constraints = "$F = $rd";
}

def MOVN_NM : InstNM<(outs GPRNM32Opnd:$rd),
                     (ins GPRNM32Opnd:$rs, GPRNM32Opnd:$rt, GPRNM32Opnd:$F),
                     "movn\t$rd, $rs, $rt",
                     []>, InstSize32 {
    let Constraints = "$F = $rd";
}

multiclass MovzPats2NM<RegisterClass CRC, RegisterClass DRC,
                     Instruction MOVZInst, Instruction XORiOp> {
  def : NMPat<(select (i32 (seteq CRC:$lhs, imm32ZExt12:$uimm12_nm)), DRC:$T,
                      DRC:$F),
              (MOVZInst DRC:$T, (XORiOp CRC:$lhs, imm32ZExt12:$uimm12_nm),
                        DRC:$F)>;
}

multiclass MovzPats0NM<RegisterClass CRC, RegisterClass DRC,
                       Instruction MOVZInst, Instruction SLTOp,
                       Instruction SLTuOp, Instruction SLTiOp,
                       Instruction SLTiuOp> {
  def : NMPat<(select (i32 (setge CRC:$lhs, CRC:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTOp CRC:$lhs, CRC:$rhs), DRC:$F)>;
  def : NMPat<(select (i32 (setuge CRC:$lhs, CRC:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTuOp CRC:$lhs, CRC:$rhs), DRC:$F)>;
  def : NMPat<(select (i32 (setge CRC:$lhs, imm32ZExt12:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTiOp CRC:$lhs, imm32ZExt12:$rhs), DRC:$F)>;
  def : NMPat<(select (i32 (setuge CRC:$lh, imm32ZExt12:$rh)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTiuOp CRC:$lh, imm32ZExt12:$rh), DRC:$F)>;
  def : NMPat<(select (i32 (setle CRC:$lhs, CRC:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTOp CRC:$rhs, CRC:$lhs), DRC:$F)>;
  def : NMPat<(select (i32 (setule CRC:$lhs, CRC:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTuOp CRC:$rhs, CRC:$lhs), DRC:$F)>;
  def : NMPat<(select (i32 (setgt CRC:$lhs, immZExt12Plus1:$rhs)),
                      DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTiOp CRC:$lhs, (Plus1 imm:$rhs)), DRC:$F)>;
  def : NMPat<(select (i32 (setugt CRC:$lhs, immZExt12Plus1:$rhs)),
                      DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTiuOp CRC:$lhs, (Plus1 imm:$rhs)), DRC:$F)>;
}

// Use MIPS conditional instruction definitions
defm : MovzPats0NM<GPRNM32, GPRNM32, MOVZ_NM, SLT_NM, SLTU_NM, SLTI_NM,
                   SLTIU_NM>;


defm : MovzPats1<GPRNM32, GPRNM32, MOVZ_NM, XOR_NM>, ISA_NANOMIPS;
defm : MovzPats2NM<GPRNM32, GPRNM32, MOVZ_NM, XORI_NM>;
defm : MovnPats<GPRNM32, GPRNM32, MOVN_NM, XOR_NM>, ISA_NANOMIPS;

def EXTW_NM : InstNM<(outs GPRNM32Opnd:$rd),
		     (ins GPRNM32Opnd:$rs, GPRNM32Opnd:$rt, uimm5:$shift),
		     "extw\t$rd, $rs, $rt, $shift", []>,
		     _Pool_POOL32A7<0b011> {
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  bits<5> shift;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
  let Inst{10...6} = shift;
}

class ROTX_Enc : _Pool_P_U12<0b1101> {
  bits<5> rt;
  bits<5> rs;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{11} = 0;
  let Inst{5} = 0;
}

class RotxAlias<string op, SDPatternOperator OpNode, bits<5> shift,
			bits<5> shiftx, bit stripe> :
      InstNM<(outs GPRNM32Opnd:$rt), (ins GPRNM32Opnd:$rs),
		     !strconcat(op, "\t$rt, $rs"),
		     [(set GPRNM32Opnd:$rt, (OpNode GPRNM32Opnd:$rs))]>,
		     ROTX_Enc {
  let Inst{10...7} = shiftx{4...1};
  let Inst{4...0} = shift;
  let Inst{6} = stripe;
}

def BITREVW_NM : RotxAlias<"bitrevw", bitreverse, 31, 0, 0>;
def BYTEREVW_NM : RotxAlias<"byterevw", bswap, 24, 8, 0>;

def ROTX_NM : InstNM<(outs GPRNM32Opnd:$rt),
		     (ins GPRNM32Opnd:$rs, uimm5:$shift,
		     uimm5s1_nm:$shiftx, uimm1:$stripe),
		     "rotx\t$rt, $rs, $shift, $shiftx, $stripe", []>,
		     ROTX_Enc {
  bits<5> shift;
  bits<5> shiftx;
  bits<1> stripe;
  let Inst{10...7} = shiftx{4...1};
  let Inst{4...0} = shift;
  let Inst{6} = stripe;
}

def BITREVB_NM : MipsInstAlias<"bitrevb $rt, $rs",
				(ROTX_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rs, 7, 8, 1)>,
				ISA_NANOMIPS;
def BITREVH_NM : MipsInstAlias<"bitrevh $rt, $rs",
				(ROTX_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rs, 15, 16, 0)>,
				ISA_NANOMIPS;
def BYTEREVH_NM : MipsInstAlias<"byterevh $rt, $rs",
				(ROTX_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rs, 8, 24, 0)>,
				ISA_NANOMIPS;
