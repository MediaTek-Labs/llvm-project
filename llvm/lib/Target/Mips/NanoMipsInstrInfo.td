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
  return (N < -256 && N >= -4095) || (N > 4095 && N <= 65535);
}]>;
def imm32ZExt12 : IntImmLeaf<i32, [{ return isUInt<12>(Imm.getZExtValue()); }]>;
def imm32ZExt12ANDI :
  IntImmLeaf<i32, [{ return isUInt<12>(Imm.getZExtValue()) ||
                            Imm.getZExtValue() == 65535; }]>;
def imm32ZExt5  : IntImmLeaf<i32, [{ return isUInt<5>(Imm.getZExtValue()); }]>;
// True if (N + 1) fits in 12-bit field.
def immZExt12Plus1 : PatLeaf<(imm), [{
  return isUInt<13>(N->getZExtValue()) && isUInt<12>(N->getZExtValue() + 1);
}]>;
def immZExt7Plus1 : PatLeaf<(imm), [{
  return isUInt<8>(N->getZExtValue()) && isUInt<7>(N->getZExtValue() + 1);
}]>;
// Immediate range covering both signed 12-bit and unsigned 16-bit immediates.
def imm32SExt12OrZExt16 : IntImmLeaf<i32, [{ return isUInt<12>(-Imm.getSExtValue())
                                           || isUInt<16>(Imm.getZExtValue()); }]>;

def UImm12AsmOperandClass : UImmAsmOperandClass<12, []>;
def UImm5AsmOperandClass : UImmAsmOperandClass<5, [UImm12AsmOperandClass]>;
def UImm3Plus1AsmOperandClass :
  ConstantUImmAsmOperandClass<3, [UImm5AsmOperandClass], 1>;

def uimm12_nm : Operand<i32> {
  let PrintMethod = "printUImm<12>";
  let ParserMatchClass = !cast<AsmOperandClass>("UImm12AsmOperandClass");
}
def uimm5_nm : Operand<i32> {
  let PrintMethod = "printUImm<5>";
  let ParserMatchClass = !cast<AsmOperandClass>("UImm5AsmOperandClass");
}
def uimm3plus1_nm : Operand<i32> {
  let PrintMethod = "printUImm<3, 1>";
  let ParserMatchClass = !cast<AsmOperandClass>("UImm3Plus1AsmOperandClass");
}

def simm32power2 : IntImmLeaf<i32, [{ return isPowerOf2_32(Imm.getSExtValue()); }]>;

def uimm16_simm12 : Operand<i32>;

def addrsimm9 : ComplexPattern<iPTR, 2, "selectIntAddrSImm9", [frameindex]>;
def addruimm12 : ComplexPattern<iPTR, 2, "selectIntAddrUImm12", [frameindex]>;
def addrindexed : ComplexPattern<iPTR, 2, "selectIntAddrIndexed", [frameindex]>;
def addrindexedlsl1 : ComplexPattern<iPTR, 2, "selectIntAddrIndexedLsl1", [frameindex]>;
def addrindexedlsl2 : ComplexPattern<iPTR, 2, "selectIntAddrIndexedLsl2", []>;

def Log2XForm : SDNodeXForm<imm, [{ return CurDAG->getTargetConstant(Log2_32(N->getZExtValue()),
                                                                   SDLoc(N), MVT::i32);
                                  }]>;

//===----------------------------------------------------------------------===//
//
// Instruction Descriptions
//
//===----------------------------------------------------------------------===//

// 32-bit arithmetic and logical instructions with 3 register operands.
class ArithLoginR32<string opstr, RegisterOperand RO,
                    SDPatternOperator OpNode = null_frag>:
  InstNM<(outs RO:$rd), (ins RO:$rs, RO:$rt),
         !strconcat(opstr, "\t$rd, $rs, $rt"),
         [(set RO:$rd, (OpNode RO:$rs, RO:$rt))]>, InstSize32;

// 16-bit arithmetic and logical instructions with 2 register operands.
class ArithLogicR16<string opstr, RegisterOperand RO,
                    SDPatternOperator OpNode = null_frag>:
    InstNM<(outs RO:$rt), (ins RO:$rs), !strconcat(opstr, "\t$rt, $rs"),
           [(set RO:$rt, (OpNode RO:$rs))]>, InstSize16;

// Arithmetic and logical instructions with 2 register operands and immediate.
class ArithLogicINM<string opstr, Operand Od, RegisterOperand RO,
                    SDPatternOperator imm_type = null_frag,
                    SDPatternOperator OpNode = null_frag> :
  InstNM<(outs RO:$rt), (ins RO:$rs, Od:$imm),
         !strconcat(opstr, "\t$rt, $rs, $imm"),
         [(set RO:$rt, (OpNode RO:$rs, imm_type:$imm))]> {
  let isReMaterializable = 1;
}

class ArithLogicI32<string opstr, Operand Od, RegisterOperand RO,
                    SDPatternOperator imm_type = null_frag,
                    SDPatternOperator OpNode = null_frag> :
  ArithLogicINM<opstr, Od, RO, imm_type, OpNode>, InstSize32;

class DivMod<string opstr, SDPatternOperator OpNode = null_frag> :
    ArithLoginR32<opstr, GPRNM32Opnd, OpNode> {
  // Need to insert TEQ, because DIV(U)/MOD(U) don't trap on division by zero.
  bit usesCustomInserter = 1;
}

class ExtBaseNM<RegisterOperand RO = GPRNM32Opnd> :
  InstNM<(outs RO:$rt), (ins RO:$rs, uimm5:$pos, uimm5_plus1:$size),
         "ext\t$rt, $rs, $pos, $size",
         [(set RO:$rt, (MipsExt RO:$rs, immZExt5:$pos, immZExt5Plus1:$size))],
         II_EXT>, InstSize32 {
  let isCodeGenOnly = 1;
}

class InsBaseNM<RegisterOperand RO = GPRNM32Opnd> :
  InstNM<(outs RO:$rt),
         (ins RO:$rs, uimm5:$pos, uimm5_inssize_plus1:$size, RO:$src),
         "ins \t$rt, $rs, $pos, $size",
         [(set RO:$rt,
               (MipsIns RO:$rs, immZExt5:$pos, immZExt5Plus1:$size, RO:$src))],
         II_INS>, InstSize32 {
  let Constraints = "$src = $rt";
  let isCodeGenOnly = 1;
}

class Trap<string opstr> :
  InstNM<(outs), (ins GPRNM32Opnd:$rs, GPRNM32Opnd:$rt, uimm5_nm:$imm),
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
  InstNM<(outs RO:$rd), (ins RO:$rt), !strconcat(opstr, "\t$rd, $rt"),
         [(set RO:$rd, (sext_inreg RO:$rt, vt))]>, InstSize32;

class LoadScaledAddressNM<string opstr, RegisterOperand RO> :
  InstNM<(outs RO:$rd), (ins RO:$rs, RO:$rt, uimm2:$shift),
         !strconcat(opstr, "\t$rd, $rs, $rt, $shift"),
         [(set RO:$rd, (add (shl RO:$rs, immZExt2:$shift), RO:$rt))]>,
  InstSize32;

class CountLeading0NM<string opstr, RegisterOperand RO>:
  InstNM<(outs RO:$rd), (ins RO:$rs), !strconcat(opstr, "\t$rd, $rs"),
         [(set RO:$rd, (ctlz RO:$rs))]>, InstSize32;

class CountLeading1NM<string opstr, RegisterOperand RO>:
  InstNM<(outs RO:$rd), (ins RO:$rs), !strconcat(opstr, "\t$rd, $rs"),
         [(set RO:$rd, (ctlz (not RO:$rs)))]>, InstSize32;

class ReverseNM<string opstr, RegisterOperand RO, SDNode OpNode>:
  InstNM<(outs RO:$rd), (ins RO:$rs), !strconcat(opstr, "\t$rd, $rs"),
         [(set RO:$rd, (OpNode RO:$rs))]>, InstSize32;

//===----------------------------------------------------------------------===//
//
// Instruction Definitions
//
//===----------------------------------------------------------------------===//

// _POOL32A0_0 pool of instructions.
def SLLV_NM  : ArithLoginR32<"sllv", GPRNM32Opnd, shl>;
def SRLV_NM  : ArithLoginR32<"srlv", GPRNM32Opnd, srl>;
def SRAV_NM  : ArithLoginR32<"srav", GPRNM32Opnd, sra>;
def ROTRV_NM : ArithLoginR32<"rotrv", GPRNM32Opnd, rotr>;
def ADD_NM   : ArithLoginR32<"add", GPRNM32Opnd>;
def ADDu_NM  : ArithLoginR32<"addu", GPRNM32Opnd, add>;
def SUB_NM   : ArithLoginR32<"sub", GPRNM32Opnd>;
def SUBu_NM  : ArithLoginR32<"subu", GPRNM32Opnd, sub>;
def AND_NM   : ArithLoginR32<"and", GPRNM32Opnd, and>;
def OR_NM    : ArithLoginR32<"or", GPRNM32Opnd, or>;
def NOR_NM   : ArithLoginR32<"nor", GPRNM32Opnd>;
def XOR_NM   : ArithLoginR32<"xor", GPRNM32Opnd, xor>;
def SLT_NM   : ArithLoginR32<"slt", GPRNM32Opnd, setlt>;
def SLTU_NM  : ArithLoginR32<"sltu", GPRNM32Opnd, setult>;
def MUL_NM   : ArithLoginR32<"mul", GPRNM32Opnd, mul>;
def MUH_NM   : ArithLoginR32<"muh", GPRNM32Opnd, mulhs>;
def MULU_NM  : ArithLoginR32<"mulu", GPRNM32Opnd>;
def MUHU_NM  : ArithLoginR32<"muhu", GPRNM32Opnd, mulhu>;
def DIV_NM   : DivMod<"div", sdiv>;
def MOD_NM   : DivMod<"mod", srem>;
def DIVU_NM  : DivMod<"divu", udiv>;
def MODU_NM  : DivMod<"modu", urem>;

// P.U12 pool of instructions
def ORI_NM   : ArithLogicI32<"ori", uimm12_nm, GPRNM32Opnd, imm32ZExt12, or>;
def XORI_NM  : ArithLogicI32<"xori", uimm12_nm, GPRNM32Opnd, imm32ZExt12, xor>;
def ANDI_NM  : ArithLogicI32<"andi", uimm16, GPRNM32Opnd, imm32ZExt12ANDI, and>;
def SLTI_NM  : ArithLogicI32<"slti", uimm12_nm, GPRNM32Opnd, imm32ZExt12, setlt>;
def SLTIU_NM : ArithLogicI32<"sltiu", uimm12_nm, GPRNM32Opnd, imm32ZExt12, setult>;

// TODO: isCodeGenOnly=1 was added to avoid conflicts with same instructions
//       for other Mips targets. Handle this when adding integrated asm
//       for NanoMips.
let isCodeGenOnly = 1 in
def SEQI_NM  : ArithLogicI32<"seqi", uimm12_nm, GPRNM32Opnd, imm32ZExt12, seteq>;

// P.SHIFT pool of instructions
def SLL_NM  : ArithLogicI32<"sll", uimm5_nm, GPRNM32Opnd, imm32ZExt5, shl>;
def SRL_NM  : ArithLogicI32<"srl", uimm5_nm, GPRNM32Opnd, imm32ZExt5, srl>;
def SRA_NM  : ArithLogicI32<"sra", uimm5_nm, GPRNM32Opnd, imm32ZExt5, sra>;
def ROTR_NM : ArithLogicI32<"rotr", uimm5_nm, GPRNM32Opnd, imm32ZExt5, rotr>;

def EXT_NM : ExtBaseNM;
def INS_NM : InsBaseNM;

let isCodeGenOnly = 1 in
def ADDIU_NM : ArithLogicI32<"addiu", uimm16_simm12, GPRNM32Opnd,
                             imm32SExt12OrZExt16, add>;
let Constraints = "$rt = $rs" in
def ADDIU48_NM : ArithLogicINM<"addiu[48]", simm32_relaxed, GPRNM32Opnd,
                               imm32_NM, add>, InstSize48;
let isCodeGenOnly = 1 in
def LSA_NM : LoadScaledAddressNM<"lsa", GPRNM32Opnd>;

def SEB_NM : SignExtInRegNM<"seb", i8, GPRNM32Opnd>;
def SEH_NM : SignExtInRegNM<"seh", i16, GPRNM32Opnd>;

def CLZ_NM : CountLeading0NM<"clz", GPRNM32Opnd>;
def CLO_NM : CountLeading1NM<"clo", GPRNM32Opnd>;

def BITREVW_NM : ReverseNM<"bitrevw", GPRNM32Opnd, bitreverse>;
def BYTEREVW_NM : ReverseNM<"byterevw", GPRNM32Opnd, bswap>;

// P.TRAP pool of instructions.
def TEQ_NM : Trap<"teq">;
def TNE_NM : Trap<"tne">;

def NOT_NM   : ArithLogicR16<"not", GPR3Opnd, not>;
let isCodeGenOnly = 1 in {
  def XOR16_NM : ArithLogicR16<"xor16", GPR3Opnd>;
  def AND16_NM : ArithLogicR16<"and16", GPR3Opnd>;
  def OR16_NM  : ArithLogicR16<"or16", GPR3Opnd>;
}

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

let isReMaterializable = 1 in {
let isCodeGenOnly = 1 in
def Li_NM : RegImmNM<"li", simm32_relaxed, GPRNM32Opnd>, InstSize48;
def LA_NM : RegImmNM<"la", simm32_relaxed, GPRNM32Opnd>, InstSize48;
} // isReMaterializable = 1

def ALUIPC_NM : RegImmNM<"aluipc", simm32_relaxed, GPRNM32Opnd>, InstSize32;

def LEA_ADDIU_NM : EffectiveAddressNM<"addiu", GPRNM32Opnd>, InstSize32;

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

def : NMPat<(i32 imm32_NM:$imm), (Li_NM imm:$imm)>;

def : NMPat<(MipsJmpLink (i32 texternalsym:$dst)), (BALC_NM texternalsym:$dst)>;

def : NMPat<(MipsFullAddr tglobaladdr:$in), (LA_NM tglobaladdr:$in)>;
def : NMPat<(MipsFullAddr tglobaltlsaddr:$in), (LA_NM tglobaltlsaddr:$in)>;
def : NMPat<(MipsFullAddr tblockaddress:$in), (LA_NM tblockaddress:$in)>;
def : NMPat<(MipsFullAddr tjumptable:$in), (LA_NM tjumptable:$in)>;
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
