//=- NanoMipsDSPInstrInfo.td - DSP ASE instructions -*- tablegen --------*-=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes NanoMips DSP ASE instructions.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// DSP instructions
//===----------------------------------------------------------------------===//

class ADDU_QB_NM_FMT<bits<8> psel>: InstNM_Enc, _Pool_P32A<0b101> {
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
  let Inst{10...3} = psel;
}

class CMPGU_PRECRQ_Desc<string opstr,
                        RegisterOperand ROIN,
                        RegisterOperand ROOUT,
                        SDPatternOperator OpNode = null_frag>:
   InstNM<(outs ROOUT:$rd), (ins ROIN:$rs, ROIN:$rt),
	 !strconcat(opstr, "\t$rd, $rs, $rt"),
	 [(set ROOUT:$rd, (OpNode ROIN:$rs, ROIN:$rt))],
         NoItinerary, 0>;

class CMPGU_PICK_PRECRQ_Enc<bits<7> psel>: _Pool_P32A<0b101> {
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
  let Inst{9...3} = psel;
}

class CMP_EQ_QB_R3_NM_FMT<bits<7> psel>: InstNM_Enc, _Pool_P32A<0b101> {
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
  let Inst{9...3} = psel;
}
class CMP_EQ_QB_R2_NM_FMT<bits<7> psel>: InstNM_Enc, _Pool_P32A<0b101> {
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{9...3} = psel;
}

class SHLL_QB_R2_NM_DESC_BASE<string instr_asm, SDPatternOperator OpNode,
                              Operand ImmOp, ImmLeaf ImmPattern,
                              bit SideEffects = 0> {
  dag OutOperandList = (outs DSPRNMOpnd:$rt);
  dag InOperandList =  (ins DSPRNMOpnd:$rs, ImmOp:$sa);
  string AsmString = !strconcat(instr_asm, "\t$rt, $rs, $sa");
  list<dag> Pattern = [(set DSPRNMOpnd:$rt, (OpNode DSPRNMOpnd:$rs, ImmPattern:$sa))];
  InstrItinClass Itinerary = NoItinerary;
  bit hasSideEffects = SideEffects;
  string BaseOpcode = instr_asm;
}

class SHLL_QB_R2_IMM3_NM_DESC_BASE<string instr_asm, SDPatternOperator OpNode,
                              bit SideEffects = 0>:
  SHLL_QB_R2_NM_DESC_BASE<instr_asm, OpNode, uimm3, immZExt3, SideEffects>;

class SHLL_QB_R2_IMM4_NM_DESC_BASE<string instr_asm, SDPatternOperator OpNode,
                              bit SideEffects = 0>:
  SHLL_QB_R2_NM_DESC_BASE<instr_asm, OpNode, uimm4, immZExt4, SideEffects>;

class RDDSP_NM_DESC_BASE<string instr_asm, SDPatternOperator OpNode,
                         InstrItinClass itin> {
  dag OutOperandList = (outs GPRNM32Opnd:$rt);
  dag InOperandList = (ins uimm7:$mask);
  string AsmString = !strconcat(instr_asm, "\t$rt, $mask");
  list<dag> Pattern = [(set GPRNM32Opnd:$rt, (OpNode timmZExt7:$mask))];
  InstrItinClass Itinerary = itin;
  string BaseOpcode = instr_asm;
  bit isMoveReg = 1;
}

class WRDSP_NM_DESC_BASE<string instr_asm, SDPatternOperator OpNode,
                         InstrItinClass itin> {
  dag OutOperandList = (outs);
  dag InOperandList = (ins GPRNM32Opnd:$rt, uimm7:$mask);
  string AsmString = !strconcat(instr_asm, "\t$rt, $mask");
  list<dag> Pattern = [(OpNode GPRNM32Opnd:$rt, timmZExt7:$mask)];
  InstrItinClass Itinerary = itin;
  string BaseOpcode = instr_asm;
  bit isMoveReg = 1;
}

class MFHI_NM_DESC_BASE<string instr_asm, RegisterOperand ROACC,
                     RegisterOperand ROD, SDNode OpNode,
                     InstrItinClass itin> {
  dag OutOperandList = (outs ROD:$rt);
  dag InOperandList = (ins ROACC:$ac);
  string AsmString = !strconcat(instr_asm, "\t$rt, $ac");
  list<dag> Pattern = [(set ROD:$rt, (OpNode ROACC:$ac))];
  InstrItinClass Itinerary = itin;
  string BaseOpcode = instr_asm;
  bit isMoveReg = 1;
}

class SHLL_QB_NM_DESC:   SHLL_QB_R2_IMM3_NM_DESC_BASE<"shll.qb", int_mips_shll_qb, 1>;
class SHRL_QB_NM_DESC:   SHLL_QB_R2_IMM3_NM_DESC_BASE<"shrl.qb", int_mips_shrl_qb>;
class SHRL_PH_NM_DESC:   SHLL_QB_R2_IMM4_NM_DESC_BASE<"shrl.ph", int_mips_shrl_ph>;
class SHLL_PH_NM_DESC:   SHLL_QB_R2_IMM4_NM_DESC_BASE<"shll.ph", int_mips_shll_ph, 1>;
class SHLL_S_PH_NM_DESC: SHLL_QB_R2_IMM4_NM_DESC_BASE<"shll_s.ph", int_mips_shll_s_ph, 1>;
class RDDSP_NM_DESC    : RDDSP_NM_DESC_BASE<"rddsp", int_mips_rddsp, NoItinerary>;
class WRDSP_NM_DESC    : WRDSP_NM_DESC_BASE<"wrdsp", int_mips_wrdsp, NoItinerary>;
class MFHI_NM_DESC<RO_TYPES RO = NanoMipsRO_TYPES> :
  MFHI_NM_DESC_BASE<"mfhi", RO.ACC64DSP, RO.GPR, MipsMFHI, NoItinerary>;
class MFLO_NM_DESC<RO_TYPES RO = NanoMipsRO_TYPES> :
  MFHI_NM_DESC_BASE<"mflo", RO.ACC64DSP, RO.GPR, MipsMFLO, NoItinerary>;

class SHRL_PH_FMT: InstNM_Enc, _Pool_P32A<0b111> {
  bits<4> sa;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...12} = sa;
  let Inst{11...3} = 0b001111111;
}

class SHLL_PH_FMT<bits<1> signed>: InstNM_Enc, _Pool_P32A<0b101> {
  bits<4> sa;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...12} = sa;
  let Inst{11} = signed;
  let Inst{10...3} = 0b01110110;
}

class SHL_QB_FMT<bits<1> signed>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<3> sa;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...13} = sa;
  let Inst{12} = signed;
  let Inst{11...3} = 0b100001111;
}

class RDWR_DSP_FMT<bits<1> psel>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<7> mask;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...14} = mask;
  let Inst{13} = 0;
  let Inst{12} = psel;
  let Inst{11...3} = 0b011001111;
}

class MOVE_HALF_ACC_FMT<bits<2> psel>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<2> ac;
  let Inst{15...14} = ac;
  let Inst{13...12} = psel;
  let Inst{11...3} = 0b000001111;
}

class MF_FMT<bits<2> psel>: MOVE_HALF_ACC_FMT<psel> {
  bits<5> rt;
  let Inst{25...21} = rt;
}

class MT_FMT<bits<2> psel>: MOVE_HALF_ACC_FMT<psel> {
  bits<5> rs;
  let Inst{20...16} = rs;
}

class MADD_NM_ENC: InstNM_Enc, _Pool_P32A<0b111> {
  bits<2> ac;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...14} = ac;
  let Inst{13...3} = 0b00101010111;
}
class ADDU_QB_NM_ENC     : ADDU_QB_NM_FMT     <0b00011001>;
class ADDU_S_QB_NM_ENC   : ADDU_QB_NM_FMT     <0b10011001>;
class SUBU_QB_NM_ENC     : ADDU_QB_NM_FMT     <0b01011001>;
class SUBU_S_QB_NM_ENC   : ADDU_QB_NM_FMT     <0b11011001>;
class ADDQ_PH_NM_ENC     : ADDU_QB_NM_FMT     <0b00000001>;
class ADDQ_S_PH_NM_ENC   : ADDU_QB_NM_FMT     <0b10000001>;
class SUBQ_PH_NM_ENC     : ADDU_QB_NM_FMT     <0b01000001>;
class SUBQ_S_PH_NM_ENC   : ADDU_QB_NM_FMT     <0b11000001>;
class MUL_PH_NM_ENC      : ADDU_QB_NM_FMT    <0b00000101>;
class MUL_S_PH_NM_ENC    : ADDU_QB_NM_FMT    <0b10000101>;
class PRECRQ_PH_W_NM_ENC : CMP_EQ_QB_R3_NM_FMT<0b0011101>;
class SHLL_QB_NM_ENC     : SHL_QB_FMT         <0b0>;
class SHRL_QB_NM_ENC     : SHL_QB_FMT         <0b1>;
class SHRL_PH_NM_ENC     : SHRL_PH_FMT;
class SHLL_PH_NM_ENC     : SHLL_PH_FMT        <0b0>;
class SHLL_S_PH_NM_ENC   : SHLL_PH_FMT        <0b1>;
class MFHI_NM_ENC        : MF_FMT             <0b00>;
class MFLO_NM_ENC        : MF_FMT             <0b01>;
class MTHI_NM_ENC        : MT_FMT             <0b10>;
class MTLO_NM_ENC        : MT_FMT             <0b11>;
class CMPGU_EQ_QB_NM_ENC : CMP_EQ_QB_R3_NM_FMT<0b0011000>;
class CMPGU_LT_QB_NM_ENC : CMP_EQ_QB_R3_NM_FMT<0b0100000>;
class CMPGU_LE_QB_NM_ENC : CMP_EQ_QB_R3_NM_FMT<0b0101000>;
class PICK_QB_NM_ENC     : CMP_EQ_QB_R3_NM_FMT<0b0111101>;
class PICK_PH_NM_ENC     : CMP_EQ_QB_R3_NM_FMT<0b1000101>;
class CMP_EQ_PH_NM_ENC   : CMP_EQ_QB_R2_NM_FMT<0b0000000>;
class CMP_LE_PH_NM_ENC   : CMP_EQ_QB_R2_NM_FMT<0b0010000>;
class CMP_LT_PH_NM_ENC   : CMP_EQ_QB_R2_NM_FMT<0b0001000>;
class RDDSP_NM_ENC       : RDWR_DSP_FMT       <0b0>;
class WRDSP_NM_ENC       : RDWR_DSP_FMT       <0b1>;

def ADDU_QB_NM     : ADDU_QB_NM_ENC    , ADDU_QB_DESC    <NanoMipsRO_TYPES>;
def ADDU_S_QB_NM   : ADDU_S_QB_NM_ENC  , ADDU_S_QB_DESC  <NanoMipsRO_TYPES>;
def SUBU_QB_NM     : SUBU_QB_NM_ENC    , SUBU_QB_DESC    <NanoMipsRO_TYPES>;
def SUBU_S_QB_NM   : SUBU_S_QB_NM_ENC  , SUBU_S_QB_DESC  <NanoMipsRO_TYPES>;
def ADDQ_PH_NM     : ADDQ_PH_NM_ENC    , ADDQ_PH_DESC    <NanoMipsRO_TYPES>;
def ADDQ_S_PH_NM   : ADDQ_S_PH_NM_ENC  , ADDQ_S_PH_DESC  <NanoMipsRO_TYPES>;
def SUBQ_PH_NM     : SUBQ_PH_NM_ENC    , SUBQ_PH_DESC    <NanoMipsRO_TYPES>;
def SUBQ_S_PH_NM   : SUBQ_S_PH_NM_ENC  , SUBQ_S_PH_DESC  <NanoMipsRO_TYPES>;
def MUL_PH_NM      : MUL_PH_NM_ENC     , MUL_PH_DESC     <NanoMipsRO_TYPES>;
def MUL_S_PH_NM    : MUL_S_PH_NM_ENC   , MUL_S_PH_DESC   <NanoMipsRO_TYPES>;
def PRECRQ_PH_W_NM : PRECRQ_PH_W_NM_ENC, PRECRQ_PH_W_DESC<NanoMipsRO_TYPES>;
def SHLL_QB_NM     : SHLL_QB_NM_ENC    , SHLL_QB_NM_DESC;
def SHRL_QB_NM     : SHRL_QB_NM_ENC    , SHRL_QB_NM_DESC;
def SHRL_PH_NM     : SHRL_PH_NM_ENC    , SHRL_PH_NM_DESC;
def SHLL_PH_NM     : SHLL_PH_NM_ENC    , SHLL_PH_NM_DESC;
def SHLL_S_PH_NM   : SHLL_S_PH_NM_ENC  , SHLL_S_PH_NM_DESC;
def MFHI_NM        : MFHI_NM_ENC       , MFHI_NM_DESC    <NanoMipsRO_TYPES>;
def MFLO_NM        : MFLO_NM_ENC       , MFLO_NM_DESC    <NanoMipsRO_TYPES>;
def MTHI_NM        : MTHI_NM_ENC       , MTHI_DESC       <NanoMipsRO_TYPES>;
def MTLO_NM        : MTLO_NM_ENC       , MTLO_DESC       <NanoMipsRO_TYPES>;
def MADD_NM        : MADD_NM_ENC       , MADD_DSP_DESC   <NanoMipsRO_TYPES>;
def CMPGU_EQ_QB_NM : CMPGU_EQ_QB_NM_ENC, CMPGU_EQ_QB_DESC<NanoMipsRO_TYPES>;
def CMPGU_LT_QB_NM : CMPGU_LT_QB_NM_ENC, CMPGU_LT_QB_DESC<NanoMipsRO_TYPES>;
def CMPGU_LE_QB_NM : CMPGU_LE_QB_NM_ENC, CMPGU_LE_QB_DESC<NanoMipsRO_TYPES>;
def CMP_EQ_PH_NM   : CMP_EQ_PH_NM_ENC  , CMP_EQ_PH_DESC  <NanoMipsRO_TYPES>;
def CMP_LE_PH_NM   : CMP_LE_PH_NM_ENC  , CMP_LE_PH_DESC  <NanoMipsRO_TYPES>;
def CMP_LT_PH_NM   : CMP_LT_PH_NM_ENC  , CMP_LT_PH_DESC  <NanoMipsRO_TYPES>;
def PICK_QB_NM     : PICK_QB_NM_ENC    , PICK_QB_DESC    <NanoMipsRO_TYPES>;
def PICK_PH_NM     : PICK_PH_NM_ENC    , PICK_PH_DESC    <NanoMipsRO_TYPES>;
def RDDSP_NM       : RDDSP_NM_ENC      , RDDSP_NM_DESC;
def WRDSP_NM       : WRDSP_NM_ENC      , WRDSP_NM_DESC;

let AdditionalPredicates = [HasDSPNanoMips, HasDSP] in {
  def PseudoMTLOHI_DSP_NM : PseudoMTLOHI<ACC64DSPNM, GPRNM32>;
}

def : BitconvertPat<i32,   v2i16, GPRNM32, DSPRNM,  HasDSPNanoMips>;
def : BitconvertPat<v2i16, i32,   DSPRNM,  GPRNM32, HasDSPNanoMips>;
def : BitconvertPat<i32,   v4i8,  GPRNM32, DSPRNM,  HasDSPNanoMips>;
def : BitconvertPat<v4i8,  i32,   DSPRNM,  GPRNM32, HasDSPNanoMips>;

def : DSPPat<(v2i16 (load addr:$a)),
             (v2i16 (COPY_TO_REGCLASS (LW_NM addr:$a), DSPRNM)), HasDSPNanoMips>;
def : DSPPat<(v4i8 (load addr:$a)),
	     (v4i8 (COPY_TO_REGCLASS (LW_NM addr:$a), DSPRNM)), HasDSPNanoMips>;
def : DSPPat<(store (v2i16 DSPRNM:$val), addr:$a),
	     (SW (COPY_TO_REGCLASS DSPRNM:$val, GPRNM32), addr:$a), HasDSPNanoMips>;
def : DSPPat<(store (v4i8 DSPRNM:$val), addr:$a),
	     (SW (COPY_TO_REGCLASS DSPRNM:$val, GPRNM32), addr:$a), HasDSPNanoMips>;

def : DSPBinPat<ADDQ_PH_NM, v2i16, int_mips_addq_ph, HasDSPNanoMips>;
def : DSPBinPat<ADDQ_PH_NM, v2i16, add             , HasDSPNanoMips>;
def : DSPBinPat<SUBQ_PH_NM, v2i16, int_mips_subq_ph, HasDSPNanoMips>;
def : DSPBinPat<SUBQ_PH_NM, v2i16, sub             , HasDSPNanoMips>;
def : DSPBinPat<MUL_PH_NM , v2i16, int_mips_mul_ph , HasDSPNanoMips>;
def : DSPBinPat<MUL_PH_NM , v2i16, mul             , HasDSPNanoMips>;
def : DSPBinPat<ADDU_QB_NM, v4i8,  int_mips_addu_qb, HasDSPNanoMips>;
def : DSPBinPat<ADDU_QB_NM, v4i8,  add             , HasDSPNanoMips>;
def : DSPBinPat<SUBU_QB_NM, v4i8,  int_mips_subu_qb, HasDSPNanoMips>;
def : DSPBinPat<SUBU_QB_NM, v4i8,  sub             , HasDSPNanoMips>;

def : DSPShiftPat<SHLL_PH_NM, v2i16, MipsSHLL_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHRL_PH_NM, v2i16, MipsSHRL_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHLL_PH_NM, v2i16, int_mips_shll_ph,  immZExt4, HasDSPNanoMips>;
def : DSPShiftPat<SHRL_PH_NM, v2i16, int_mips_shrl_ph,  immZExt4, HasDSPNanoMips>;
def : DSPShiftPat<SHLL_QB_NM, v4i8,  MipsSHLL_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHRL_QB_NM, v4i8,  MipsSHRL_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHLL_QB_NM, v4i8,  int_mips_shll_qb,  immZExt3, HasDSPNanoMips>;
def : DSPShiftPat<SHRL_QB_NM, v4i8,  int_mips_shrl_qb,  immZExt3, HasDSPNanoMips>;
