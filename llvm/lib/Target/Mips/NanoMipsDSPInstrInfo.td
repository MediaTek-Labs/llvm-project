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

class SHLLV_S_W_NM_FMT<bits<7> psel>: InstNM_Enc, _Pool_P32A<0b101> {
  bits<5> rd;
  bits<5> rs_sa;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs_sa;
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

class ABSQ_S_NM_FMT<bits<7> psel>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<5> rt; // input register, rs is the new name,
              // use rt to match Mips definitions
  bits<5> rd; // output register, rt is the new name,
              // use rd to match Mips definitions
  let Inst{25...21} = rd;
  let Inst{20...16} = rt;
  let Inst{15...9} = psel;
  let Inst{8...3} = 0b100111;
}

class RADDU_W_QB_NM_ENC: InstNM_Enc, _Pool_P32A<0b111> {
  bits<5> rs;
  bits<5> rd; // output register, rt is the new name,
              // use rd to match Mips definitions
  let Inst{25...21} = rd;
  let Inst{20...16} = rs;
  let Inst{15...9} = 0b1111000;
  let Inst{8...3} = 0b100111;
}

class DPA_MAQ_NM_FMT<bits<3> op1, bits<5> op2>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<5> rt;
  bits<5> rs;
  bits<2> ac;

  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...14} = ac;
  let Inst{13...9} = op2;
  let Inst{8...6} = op1;
  let Inst{5...3} = 0b111;
}

class DPA_NM_FMT<bits<5> op>: DPA_MAQ_NM_FMT<0b010, op>;
class MAQ_NM_FMT<bits<5> op>: DPA_MAQ_NM_FMT<0b001, op>;

class EXTR_W_TY1_NM_FMT<bits<3> op1, bits<5> op2>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<5> rt;
  bits<2> ac;
  bits<5> shift_rs;

  let Inst{25...21} = rt;
  let Inst{20...16} = shift_rs;
  let Inst{15...14} = ac;
  let Inst{13...9} = op2;
  let Inst{8...6} = op1;
  let Inst{5...3} = 0b111;
}

class BITREV_FMT<bits<7> op>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<5> rt; // input register, rs is the new name,
              // use rt to match Mips definitions
  bits<5> rd; // output register, rt is the new name,
              // use rd to match Mips definitions
  let Inst{25...21} = rd;
  let Inst{20...16} = rt;
  let Inst{15...9} = op;
  let Inst{8...3} = 0b100111;
}

class INSV_NM_FMT<bits<7> op>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<5> rt;
  bits<5> rs;

  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...9} = op;
  let Inst{8...3} = 0b100111;
}

class EXTR_W_TY1_VAR_NM_FMT<bits<5> op> : EXTR_W_TY1_NM_FMT<0b010, op>;
class EXTR_W_TY1_LIT_NM_FMT<bits<5> op> : EXTR_W_TY1_NM_FMT<0b001, op>;

class SHLL_S_W_NM_FMT <bits<7> psel> : InstNM_Enc, _Pool_P32A<0b101> {

  bits<5> rt; // input register, rs is the new name,
              // use rt to match Mips definitions
  bits<5> rd; // output register, rt is the new name,
              // use rd to match Mips definitions
  bits<5> rs_sa; //shift amount, sa is the new name,
                 // use rs_sa to match Mips definitions

  let Inst{25-21} = rd;
  let Inst{20-16} = rt;
  let Inst{15-11} = rs_sa;
  let Inst{9-3} = psel;
}

class APPEND_NM_ENC : InstNM_Enc, _Pool_P32A<0b101> {
  bits<5> rt;
  bits<5> rs;
  bits<5> sa;

  let Inst{25-21} = rt;
  let Inst{20-16} = rs;
  let Inst{15-11} = sa;
  let Inst{9-3} = 0b1000010;
}

class REPL_PH_NM_ENC  : InstNM_Enc, _Pool_P32A<0b101> {
  bits<5> rd; // output register, rt is the new name,
              // use rd to match Mips definitions
  bits<10> imm; //s is the new name
  let Inst{25-21} = rd;
  let Inst{20-11} = imm;
  let Inst{9-3} = 0b0000111;
}

class REPL_QB_NM_ENC  : InstNM_Enc, _Pool_P32A<0b111> {
  bits<5> rd; // output register, rt is the new name,
              // use rd to match Mips definitions
  bits<8> imm; //u is the new name
  let Inst{25-21} = rd;
  let Inst{20-13} = imm;
  let Inst{11-3} = 0b010111111;
}

class BPOSGE32C_NM_ENC : InstNM_Enc, _Pool_P32<0b10010> {
  bits<15> offset;

  let Inst{20-14} = 0b0010001;
  let Inst{13...1} = offset{13...1};
  let Inst{0} =  offset{14};
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
}

class WRDSP_NM_DESC_BASE<string instr_asm, SDPatternOperator OpNode,
                         InstrItinClass itin> {
  dag OutOperandList = (outs);
  dag InOperandList = (ins GPRNM32Opnd:$rt, uimm7:$mask);
  string AsmString = !strconcat(instr_asm, "\t$rt, $mask");
  list<dag> Pattern = [(OpNode GPRNM32Opnd:$rt, timmZExt7:$mask)];
  InstrItinClass Itinerary = itin;
  string BaseOpcode = instr_asm;
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

class SHRA_QB_FMT<bits<1> round>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<5> rd; // output register, rt is the new name,
              // use rd to match Mips definitions
  bits<5> rt; // input register, rs is the new name,
              // use rt to match Mips definitions
  bits<3> rs_sa; //shift amount, sa is the new name,
                 // use rs_sa to match Mips definitions
  let Inst{25...21} = rd;
  let Inst{20...16} = rt;
  let Inst{15...13} = rs_sa;
  let Inst{12} = round;
  let Inst{11...3} = 0b000111111;
}

class SHRAV_NM_FMT<bits<8> psel>: InstNM_Enc, _Pool_P32A<0b101> {
  bits<5> rd;
  bits<5> rt;
  bits<5> rs_sa; //shift amount register, rs is the new name,
                 // use rs_sa to match Mips definitions
  let Inst{25...21} = rt;
  let Inst{20...16} = rs_sa;
  let Inst{15...11} = rd;
  let Inst{10...3} = psel;
}

class SHRA_PH_FMT<bits<1> round>: InstNM_Enc, _Pool_P32A<0b101> {
  bits<5> rd; // output register, rt is the new name,
              // use rd to match Mips definitions
  bits<5> rt; // input register, rs is the new name,
              // use rt to match Mips definitions
  bits<4> rs_sa; //shift amount, sa is the new name,
                 // use rs_sa to match Mips definitions
  let Inst{25...21} = rd;
  let Inst{20...16} = rt;
  let Inst{15...12} = rs_sa;
  let Inst{10} = round;
  let Inst{9...3} = 0b1100110;
}

class PRECR_SRA_FMT<bits<1> round>: InstNM_Enc, _Pool_P32A<0b101> {
  bits<5> rt;
  bits<5> rs;
  bits<5> sa;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = sa;
  let Inst{10} = round;
  let Inst{9...3} = 0b1111001;
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

class SHILO_R1_NM_FMT :  InstNM_Enc, _Pool_P32A<0b101> {
  bits<2> ac;
  bits<6> shift;
  let Inst{21-16} = shift;
  let Inst{15-14} = ac;
  let Inst{9-3} = 0b0000011;
}

class SHILO_R2_NM_FMT<bits<2> op> :  InstNM_Enc, _Pool_P32A<0b111> {
  bits<2> ac;
  bits<5> rs;

  let Inst{20-16} = rs;
  let Inst{15-14} = ac;
  let Inst{13-12} = op;
  let Inst{11-3} = 0b001001111;
}

class MADD_DSP_NM_ENC<bits<5> psel>: InstNM_Enc, _Pool_P32A<0b111> {
  bits<2> ac;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...14} = ac;
  let Inst{13...9} = psel;
  let Inst{8...3} = 0b010111;
}
class ABSQ_S_PH_NM_ENC   : ABSQ_S_NM_FMT      <0b0001000>;
class ABSQ_S_QB_NM_ENC      : ABSQ_S_NM_FMT   <0b0000000>;
class ABSQ_S_W_NM_ENC       : ABSQ_S_NM_FMT   <0b0010000>;
class PRECEQ_W_PHL_NM_ENC   : ABSQ_S_NM_FMT   <0b0101000>;
class PRECEQ_W_PHR_NM_ENC   : ABSQ_S_NM_FMT   <0b0110000>;
class PRECEQU_PH_QBL_NM_ENC : ABSQ_S_NM_FMT   <0b0111000>;
class PRECEQU_PH_QBLA_NM_ENC: ABSQ_S_NM_FMT   <0b0111001>;
class PRECEQU_PH_QBR_NM_ENC : ABSQ_S_NM_FMT   <0b1001000>;
class PRECEQU_PH_QBRA_NM_ENC: ABSQ_S_NM_FMT   <0b1001001>;
class PRECEU_PH_QBL_NM_ENC  : ABSQ_S_NM_FMT   <0b1011000>;
class PRECEU_PH_QBLA_NM_ENC : ABSQ_S_NM_FMT   <0b1011001>;
class PRECEU_PH_QBR_NM_ENC  : ABSQ_S_NM_FMT   <0b1101000>;
class PRECEU_PH_QBRA_NM_ENC : ABSQ_S_NM_FMT   <0b1101001>;
class REPLV_PH_NM_ENC       : ABSQ_S_NM_FMT   <0b0000001>;
class REPLV_QB_NM_ENC       : ABSQ_S_NM_FMT   <0b0001001>;
class DPA_W_PH_NM_ENC       : DPA_NM_FMT      <0b00000>;
class DPAQ_S_W_PH_NM_ENC    : DPA_NM_FMT      <0b00001>;
class DPAQ_SA_L_W_NM_ENC    : DPA_NM_FMT      <0b01001>;
class DPAQX_S_W_PH_NM_ENC   : DPA_NM_FMT      <0b10001>;
class DPAQX_SA_W_PH_NM_ENC  : DPA_NM_FMT      <0b11001>;
class DPAU_H_QBL_NM_ENC     : DPA_NM_FMT      <0b10000>;
class DPAU_H_QBR_NM_ENC     : DPA_NM_FMT      <0b11000>;
class DPAX_W_PH_NM_ENC      : DPA_NM_FMT      <0b01000>;
class DPS_W_PH_NM_ENC       : DPA_NM_FMT      <0b00010>;
class DPSQ_S_W_PH_NM_ENC    : DPA_NM_FMT      <0b00011>;
class DPSQ_SA_L_W_NM_ENC    : DPA_NM_FMT      <0b01011>;
class DPSQX_S_W_PH_NM_ENC   : DPA_NM_FMT      <0b10011>;
class DPSQX_SA_W_PH_NM_ENC  : DPA_NM_FMT      <0b11011>;
class DPSU_H_QBL_NM_ENC     : DPA_NM_FMT      <0b10010>;
class DPSU_H_QBR_NM_ENC     : DPA_NM_FMT      <0b11010>;
class DPSX_W_PH_NM_ENC      : DPA_NM_FMT      <0b01010>;
class MULSA_W_PH_NM_ENC     : DPA_NM_FMT      <0b10110>;
class MULSAQ_S_W_PH_NM_ENC  : DPA_NM_FMT      <0b11110>;
class MAQ_S_W_PHL_NM_ENC    : MAQ_NM_FMT      <0b01101>;
class MAQ_SA_W_PHL_NM_ENC   : MAQ_NM_FMT      <0b11101>;
class MAQ_S_W_PHR_NM_ENC    : MAQ_NM_FMT      <0b00101>;
class MAQ_SA_W_PHR_NM_ENC   : MAQ_NM_FMT      <0b10101>;
class ADDU_QB_NM_ENC     : ADDU_QB_NM_FMT     <0b00011001>;
class ADDU_S_QB_NM_ENC   : ADDU_QB_NM_FMT     <0b10011001>;
class SUBU_QB_NM_ENC     : ADDU_QB_NM_FMT     <0b01011001>;
class SUBU_S_QB_NM_ENC   : ADDU_QB_NM_FMT     <0b11011001>;
class ADDU_PH_NM_ENC     : ADDU_QB_NM_FMT     <0b00100001>;
class ADDU_S_PH_NM_ENC   : ADDU_QB_NM_FMT     <0b10100001>;
class ADDUH_QB_NM_ENC    : ADDU_QB_NM_FMT     <0b00101001>;
class ADDUH_R_QB_NM_ENC  : ADDU_QB_NM_FMT     <0b10101001>;
class SUBU_PH_NM_ENC     : ADDU_QB_NM_FMT     <0b01100001>;
class SUBU_S_PH_NM_ENC   : ADDU_QB_NM_FMT     <0b11100001>;
class SUBUH_QB_NM_ENC    : ADDU_QB_NM_FMT     <0b01101001>;
class SUBUH_R_QB_NM_ENC  : ADDU_QB_NM_FMT     <0b11101001>;
class ADDQ_PH_NM_ENC     : ADDU_QB_NM_FMT     <0b00000001>;
class ADDQ_S_PH_NM_ENC   : ADDU_QB_NM_FMT     <0b10000001>;
class ADDQH_PH_NM_ENC    : ADDU_QB_NM_FMT     <0b00001001>;
class ADDQH_R_PH_NM_ENC  : ADDU_QB_NM_FMT     <0b10001001>;
class ADDQH_W_NM_ENC     : ADDU_QB_NM_FMT     <0b00010001>;
class ADDQH_R_W_NM_ENC   : ADDU_QB_NM_FMT     <0b10010001>;
class SUBQ_PH_NM_ENC     : ADDU_QB_NM_FMT     <0b01000001>;
class SUBQ_S_PH_NM_ENC   : ADDU_QB_NM_FMT     <0b11000001>;
class SUBQH_PH_NM_ENC    : ADDU_QB_NM_FMT     <0b01001001>;
class SUBQH_R_PH_NM_ENC  : ADDU_QB_NM_FMT     <0b11001001>;
class SUBQH_W_NM_ENC     : ADDU_QB_NM_FMT     <0b01010001>;
class SUBQH_R_W_NM_ENC   : ADDU_QB_NM_FMT     <0b11010001>;
class MUL_PH_NM_ENC      : ADDU_QB_NM_FMT     <0b00000101>;
class MUL_S_PH_NM_ENC    : ADDU_QB_NM_FMT     <0b10000101>;
class SHRAV_PH_NM_ENC    : SHRAV_NM_FMT       <0b00110001>;
class SHRAV_R_PH_NM_ENC  : SHRAV_NM_FMT       <0b10110001>;
class SHRAV_QB_NM_ENC    : SHRAV_NM_FMT       <0b00111001>;
class SHRAV_R_QB_NM_ENC  : SHRAV_NM_FMT       <0b10111001>;
class SHLLV_PH_NM_ENC    : SHRAV_NM_FMT       <0b01110001>;
class SHLLV_S_PH_NM_ENC  : SHRAV_NM_FMT       <0b11110001>;
class SHRA_R_W_NM_ENC    : SHLL_S_W_NM_FMT    <0b1011110>;
class SHLL_S_W_NM_ENC    : SHLL_S_W_NM_FMT    <0b1111110>;

class PRECRQ_PH_W_NM_ENC      : CMP_EQ_QB_R3_NM_FMT<0b0011101>;
class PRECR_QB_PH_NM_ENC      : CMP_EQ_QB_R3_NM_FMT<0b0001101>;
class PRECRQ_QB_PH_NM_ENC     : CMP_EQ_QB_R3_NM_FMT<0b0010101>;
class PRECRQU_S_QB_PH_NM_ENC  : CMP_EQ_QB_R3_NM_FMT<0b0101101>;
class PRECRQ_RS_PH_W_NM_ENC   : CMP_EQ_QB_R3_NM_FMT<0b0100101>;

class PRECR_SRA_PH_W_NM_ENC   : PRECR_SRA_FMT<0b0>;
class PRECR_SRA_R_PH_W_NM_ENC : PRECR_SRA_FMT<0b1>;

class SHLL_QB_NM_ENC     : SHL_QB_FMT         <0b0>;
class SHRL_QB_NM_ENC     : SHL_QB_FMT         <0b1>;
class SHRL_PH_NM_ENC     : SHRL_PH_FMT;
class SHLL_PH_NM_ENC     : SHLL_PH_FMT        <0b0>;
class SHLL_S_PH_NM_ENC   : SHLL_PH_FMT        <0b1>;
class SHRA_QB_NM_ENC     : SHRA_QB_FMT        <0b0>;
class SHRA_R_QB_NM_ENC   : SHRA_QB_FMT        <0b1>;
class SHRA_PH_NM_ENC     : SHRA_PH_FMT        <0b0>;
class SHRA_R_PH_NM_ENC   : SHRA_PH_FMT        <0b1>;
class SHLLV_S_W_NM_ENC   : SHLLV_S_W_NM_FMT   <0b1111010>;
class SHRLV_PH_NM_ENC    : SHLLV_S_W_NM_FMT   <0b1100010>;
class SHRLV_QB_NM_ENC    : SHLLV_S_W_NM_FMT   <0b1101010>;
class SHRAV_R_W_NM_ENC   : SHLLV_S_W_NM_FMT   <0b1011010>;
class SHLLV_QB_NM_ENC    : SHLLV_S_W_NM_FMT   <0b1110010>;

class MFHI_NM_ENC        : MF_FMT             <0b00>;
class MFLO_NM_ENC        : MF_FMT             <0b01>;
class MTHI_NM_ENC        : MT_FMT             <0b10>;
class MTLO_NM_ENC        : MT_FMT             <0b11>;
class MTHLIP_NM_ENC      : SHILO_R2_NM_FMT    <0b00>;
class SHILOV_NM_ENC      : SHILO_R2_NM_FMT    <0b01>;
class SHILO_NM_ENC       : SHILO_R1_NM_FMT;
class CMPGDU_EQ_QB_NM_ENC   : CMP_EQ_QB_R3_NM_FMT<0b0110000>;
class CMPGDU_LE_QB_NM_ENC   : CMP_EQ_QB_R3_NM_FMT<0b1000000>;
class CMPGDU_LT_QB_NM_ENC   : CMP_EQ_QB_R3_NM_FMT<0b0111000>;
class CMPGU_EQ_QB_NM_ENC    : CMP_EQ_QB_R3_NM_FMT<0b0011000>;
class CMPGU_LT_QB_NM_ENC    : CMP_EQ_QB_R3_NM_FMT<0b0100000>;
class CMPGU_LE_QB_NM_ENC    : CMP_EQ_QB_R3_NM_FMT<0b0101000>;
class PICK_QB_NM_ENC        : CMP_EQ_QB_R3_NM_FMT<0b0111101>;
class PICK_PH_NM_ENC        : CMP_EQ_QB_R3_NM_FMT<0b1000101>;
class CMP_EQ_PH_NM_ENC      : CMP_EQ_QB_R2_NM_FMT<0b0000000>;
class CMP_LE_PH_NM_ENC      : CMP_EQ_QB_R2_NM_FMT<0b0010000>;
class CMP_LT_PH_NM_ENC      : CMP_EQ_QB_R2_NM_FMT<0b0001000>;
class CMPU_EQ_QB_NM_ENC     : CMP_EQ_QB_R2_NM_FMT<0b1001000>;
class CMPU_LE_QB_NM_ENC     : CMP_EQ_QB_R2_NM_FMT<0b1011000>;
class CMPU_LT_QB_NM_ENC     : CMP_EQ_QB_R2_NM_FMT<0b1010000>;
class PACKRL_PH_NM_ENC      : CMP_EQ_QB_R3_NM_FMT<0b0110101>;
class MODSUB_NM_ENC         : CMP_EQ_QB_R3_NM_FMT<0b1010010>;
class SUBQ_S_W_NM_ENC       : CMP_EQ_QB_R3_NM_FMT<0b1101000>;
class MULEQ_S_W_PHL_NM_ENC  : CMP_EQ_QB_R3_NM_FMT<0b0000100>;
class MULEQ_S_W_PHR_NM_ENC  : CMP_EQ_QB_R3_NM_FMT<0b0001100>;
class MULEU_S_PH_QBL_NM_ENC : CMP_EQ_QB_R3_NM_FMT<0b0010010>;
class MULEU_S_PH_QBR_NM_ENC : CMP_EQ_QB_R3_NM_FMT<0b0011010>;
class MULQ_RS_PH_NM_ENC     : CMP_EQ_QB_R3_NM_FMT<0b0100010>;
class MULQ_RS_W_NM_ENC      : CMP_EQ_QB_R3_NM_FMT<0b0110010>;
class MULQ_S_PH_NM_ENC      : CMP_EQ_QB_R3_NM_FMT<0b0101010>;
class MULQ_S_W_NM_ENC       : CMP_EQ_QB_R3_NM_FMT<0b0111010>;
class ADDQ_S_W_NM_ENC       : CMP_EQ_QB_R3_NM_FMT<0b1100000>;
class ADDSC_NM_ENC          : CMP_EQ_QB_R3_NM_FMT<0b1110000>;
class ADDWC_NM_ENC          : CMP_EQ_QB_R3_NM_FMT<0b1111000>;
class RDDSP_NM_ENC       : RDWR_DSP_FMT       <0b0>;
class WRDSP_NM_ENC       : RDWR_DSP_FMT       <0b1>;

class EXTRV_R_W_NM_ENC  : EXTR_W_TY1_VAR_NM_FMT<0b01111>;
class EXTR_R_W_NM_ENC   : EXTR_W_TY1_LIT_NM_FMT<0b01111>;
class EXTRV_RS_W_NM_ENC : EXTR_W_TY1_VAR_NM_FMT<0b10111>;
class EXTR_RS_W_NM_ENC  : EXTR_W_TY1_LIT_NM_FMT<0b10111>;
class EXTR_S_H_NM_ENC   : EXTR_W_TY1_LIT_NM_FMT<0b11111>;
class EXTRV_S_H_NM_ENC  : EXTR_W_TY1_VAR_NM_FMT<0b11111>;
class EXTRV_W_NM_ENC    : EXTR_W_TY1_VAR_NM_FMT<0b00111>;
class EXTR_W_NM_ENC     : EXTR_W_TY1_LIT_NM_FMT<0b00111>;
class EXTPV_NM_ENC      : EXTR_W_TY1_VAR_NM_FMT<0b10100>;
class EXTP_NM_ENC       : EXTR_W_TY1_LIT_NM_FMT<0b10011>;
class EXTPDPV_NM_ENC    : EXTR_W_TY1_VAR_NM_FMT<0b11100>;
class EXTPDP_NM_ENC     : EXTR_W_TY1_LIT_NM_FMT<0b11011>;
class BITREV_NM_ENC     : BITREV_FMT           <0b0011000>;
class INSV_NM_ENC       : INSV_NM_FMT          <0b0100000>;

def ABSQ_S_PH_NM         : ABSQ_S_PH_NM_ENC        , ABSQ_S_PH_DESC  <NanoMipsRO_TYPES>;
def ABSQ_S_QB_NM         : ABSQ_S_QB_NM_ENC        , ABSQ_S_QB_DESC  <NanoMipsRO_TYPES>;
def ABSQ_S_W_NM          : ABSQ_S_W_NM_ENC         , ABSQ_S_W_DESC   <NanoMipsRO_TYPES>;
def PRECEQ_W_PHL_NM      : PRECEQ_W_PHL_NM_ENC     , PRECEQ_W_PHL_DESC   <NanoMipsRO_TYPES>;
def PRECEQ_W_PHR_NM      : PRECEQ_W_PHR_NM_ENC     , PRECEQ_W_PHR_DESC   <NanoMipsRO_TYPES>;
def PRECEQU_PH_QBL_NM    : PRECEQU_PH_QBL_NM_ENC   , PRECEQU_PH_QBL_DESC <NanoMipsRO_TYPES>;
def PRECEQU_PH_QBLA_NM   : PRECEQU_PH_QBLA_NM_ENC  , PRECEQU_PH_QBLA_DESC<NanoMipsRO_TYPES>;
def PRECEQU_PH_QBR_NM    : PRECEQU_PH_QBR_NM_ENC   , PRECEQU_PH_QBR_DESC <NanoMipsRO_TYPES>;
def PRECEQU_PH_QBRA_NM   : PRECEQU_PH_QBRA_NM_ENC  , PRECEQU_PH_QBRA_DESC<NanoMipsRO_TYPES>;
def PRECEU_PH_QBL_NM     : PRECEU_PH_QBL_NM_ENC    , PRECEU_PH_QBL_DESC  <NanoMipsRO_TYPES>;
def PRECEU_PH_QBLA_NM    : PRECEU_PH_QBLA_NM_ENC   , PRECEU_PH_QBLA_DESC <NanoMipsRO_TYPES>;
def PRECEU_PH_QBR_NM     : PRECEU_PH_QBR_NM_ENC    , PRECEU_PH_QBR_DESC  <NanoMipsRO_TYPES>;
def PRECEU_PH_QBRA_NM    : PRECEU_PH_QBRA_NM_ENC   , PRECEU_PH_QBRA_DESC <NanoMipsRO_TYPES>;

def DPA_W_PH_NM      : DPA_W_PH_NM_ENC      , DPA_W_PH_DESC     <NanoMipsRO_TYPES>;
def DPAQ_S_W_PH_NM   : DPAQ_S_W_PH_NM_ENC   , DPAQ_S_W_PH_DESC  <NanoMipsRO_TYPES>;
def DPAQ_SA_L_W_NM   : DPAQ_SA_L_W_NM_ENC   , DPAQ_SA_L_W_DESC  <NanoMipsRO_TYPES>;
def DPAQX_S_W_PH_NM  : DPAQX_S_W_PH_NM_ENC  , DPAQX_S_W_PH_DESC <NanoMipsRO_TYPES>;
def DPAQX_SA_W_PH_NM : DPAQX_SA_W_PH_NM_ENC , DPAQX_SA_W_PH_DESC<NanoMipsRO_TYPES>;
def DPAU_H_QBL_NM    : DPAU_H_QBL_NM_ENC    , DPAU_H_QBL_DESC   <NanoMipsRO_TYPES>;
def DPAU_H_QBR_NM    : DPAU_H_QBR_NM_ENC    , DPAU_H_QBR_DESC   <NanoMipsRO_TYPES>;
def DPAX_W_PH_NM     : DPAX_W_PH_NM_ENC     , DPAX_W_PH_DESC    <NanoMipsRO_TYPES>;
def DPS_W_PH_NM      : DPS_W_PH_NM_ENC      , DPS_W_PH_DESC     <NanoMipsRO_TYPES>;
def DPSQ_S_W_PH_NM   : DPSQ_S_W_PH_NM_ENC   , DPSQ_S_W_PH_DESC  <NanoMipsRO_TYPES>;
def DPSQ_SA_L_W_NM   : DPSQ_SA_L_W_NM_ENC   , DPSQ_SA_L_W_DESC  <NanoMipsRO_TYPES>;
def DPSQX_S_W_PH_NM  : DPSQX_S_W_PH_NM_ENC  , DPSQX_S_W_PH_DESC <NanoMipsRO_TYPES>;
def DPSQX_SA_W_PH_NM : DPSQX_SA_W_PH_NM_ENC , DPSQX_SA_W_PH_DESC<NanoMipsRO_TYPES>;
def DPSU_H_QBL_NM    : DPSU_H_QBL_NM_ENC    , DPSU_H_QBL_DESC   <NanoMipsRO_TYPES>;
def DPSU_H_QBR_NM    : DPSU_H_QBR_NM_ENC    , DPSU_H_QBR_DESC   <NanoMipsRO_TYPES>;
def DPSX_W_PH_NM     : DPSX_W_PH_NM_ENC     , DPSX_W_PH_DESC    <NanoMipsRO_TYPES>;
def MAQ_S_W_PHL_NM   : MAQ_S_W_PHL_NM_ENC   , MAQ_S_W_PHL_DESC  <NanoMipsRO_TYPES>;
def MAQ_SA_W_PHL_NM  : MAQ_SA_W_PHL_NM_ENC  , MAQ_SA_W_PHL_DESC <NanoMipsRO_TYPES>;
def MAQ_S_W_PHR_NM   : MAQ_S_W_PHR_NM_ENC   , MAQ_S_W_PHR_DESC  <NanoMipsRO_TYPES>;
def MAQ_SA_W_PHR_NM  : MAQ_SA_W_PHR_NM_ENC  , MAQ_SA_W_PHR_DESC <NanoMipsRO_TYPES>;
def MULSA_W_PH_NM    : MULSA_W_PH_NM_ENC    , MULSA_W_PH_DESC   <NanoMipsRO_TYPES>;
def MULSAQ_S_W_PH_NM : MULSAQ_S_W_PH_NM_ENC , MULSAQ_S_W_PH_DESC<NanoMipsRO_TYPES>;

def ADDU_PH_NM     : ADDU_PH_NM_ENC          , ADDU_PH_DESC    <NanoMipsRO_TYPES>;
def ADDU_S_PH_NM   : ADDU_S_PH_NM_ENC        , ADDU_S_PH_DESC  <NanoMipsRO_TYPES>;
def SUBU_PH_NM     : SUBU_PH_NM_ENC          , SUBU_PH_DESC    <NanoMipsRO_TYPES>;
def SUBU_S_PH_NM   : SUBU_S_PH_NM_ENC        , SUBU_S_PH_DESC  <NanoMipsRO_TYPES>;
def ADDU_QB_NM     : ADDU_QB_NM_ENC          , ADDU_QB_DESC    <NanoMipsRO_TYPES>;
def ADDU_S_QB_NM   : ADDU_S_QB_NM_ENC        , ADDU_S_QB_DESC  <NanoMipsRO_TYPES>;
def ADDUH_QB_NM    : ADDUH_QB_NM_ENC         , ADDUH_QB_DESC   <NanoMipsRO_TYPES>;
def ADDUH_R_QB_NM  : ADDUH_R_QB_NM_ENC       , ADDUH_R_QB_DESC <NanoMipsRO_TYPES>;
def SUBU_QB_NM     : SUBU_QB_NM_ENC          , SUBU_QB_DESC    <NanoMipsRO_TYPES>;
def SUBU_S_QB_NM   : SUBU_S_QB_NM_ENC        , SUBU_S_QB_DESC  <NanoMipsRO_TYPES>;
def SUBUH_QB_NM    : SUBUH_QB_NM_ENC         , SUBUH_QB_DESC   <NanoMipsRO_TYPES>;
def SUBUH_R_QB_NM  : SUBUH_R_QB_NM_ENC       , SUBUH_R_QB_DESC <NanoMipsRO_TYPES>;
def ADDQ_PH_NM     : ADDQ_PH_NM_ENC          , ADDQ_PH_DESC    <NanoMipsRO_TYPES>;
def ADDQ_S_PH_NM   : ADDQ_S_PH_NM_ENC        , ADDQ_S_PH_DESC  <NanoMipsRO_TYPES>;
def ADDQH_PH_NM    : ADDQH_PH_NM_ENC         , ADDQH_PH_DESC   <NanoMipsRO_TYPES>;
def ADDQH_R_PH_NM  : ADDQH_R_PH_NM_ENC       , ADDQH_R_PH_DESC <NanoMipsRO_TYPES>;
def ADDQH_W_NM     : ADDQH_W_NM_ENC          , ADDQH_W_DESC    <NanoMipsRO_TYPES>;
def ADDQH_R_W_NM   : ADDQH_R_W_NM_ENC        , ADDQH_R_W_DESC  <NanoMipsRO_TYPES>;
def SUBQ_PH_NM     : SUBQ_PH_NM_ENC          , SUBQ_PH_DESC    <NanoMipsRO_TYPES>;
def SUBQ_S_PH_NM   : SUBQ_S_PH_NM_ENC        , SUBQ_S_PH_DESC  <NanoMipsRO_TYPES>;
def SUBQ_S_W_NM    : SUBQ_S_W_NM_ENC         , SUBQ_S_W_DESC   <NanoMipsRO_TYPES>;
def SUBQH_PH_NM    : SUBQH_PH_NM_ENC         , SUBQH_PH_DESC   <NanoMipsRO_TYPES>;
def SUBQH_R_PH_NM  : SUBQH_R_PH_NM_ENC       , SUBQH_R_PH_DESC <NanoMipsRO_TYPES>;
def SUBQH_W_NM     : SUBQH_W_NM_ENC          , SUBQH_W_DESC    <NanoMipsRO_TYPES>;
def SUBQH_R_W_NM   : SUBQH_R_W_NM_ENC        , SUBQH_R_W_DESC  <NanoMipsRO_TYPES>;
def ADDQ_S_W_NM    : ADDQ_S_W_NM_ENC         , ADDQ_S_W_DESC   <NanoMipsRO_TYPES>;
def ADDSC_NM       : ADDSC_NM_ENC            , ADDSC_DESC      <NanoMipsRO_TYPES>;
def ADDWC_NM       : ADDWC_NM_ENC            , ADDWC_DESC      <NanoMipsRO_TYPES>;

def MULEQ_S_W_PHL_NM   : MULEQ_S_W_PHL_NM_ENC  , MULEQ_S_W_PHL_DESC  <NanoMipsRO_TYPES>;
def MULEQ_S_W_PHR_NM   : MULEQ_S_W_PHR_NM_ENC  , MULEQ_S_W_PHR_DESC  <NanoMipsRO_TYPES>;
def MULEU_S_PH_QBL_NM  : MULEU_S_PH_QBL_NM_ENC , MULEU_S_PH_QBL_DESC <NanoMipsRO_TYPES>;
def MULEU_S_PH_QBR_NM  : MULEU_S_PH_QBR_NM_ENC , MULEU_S_PH_QBR_DESC <NanoMipsRO_TYPES>;
def MULQ_RS_PH_NM      : MULQ_RS_PH_NM_ENC     , MULQ_RS_PH_DESC     <NanoMipsRO_TYPES>;
def MULQ_RS_W_NM       : MULQ_RS_W_NM_ENC      , MULQ_RS_W_DESC      <NanoMipsRO_TYPES>;
def MULQ_S_PH_NM       : MULQ_S_PH_NM_ENC      , MULQ_S_PH_DESC      <NanoMipsRO_TYPES>;
def MULQ_S_W_NM        : MULQ_S_W_NM_ENC       , MULQ_S_W_DESC       <NanoMipsRO_TYPES>;

def MODSUB_NM      : MODSUB_NM_ENC           , MODSUB_DESC     <NanoMipsRO_TYPES>;
def MUL_PH_NM      : MUL_PH_NM_ENC           , MUL_PH_DESC     <NanoMipsRO_TYPES>;
def MUL_S_PH_NM    : MUL_S_PH_NM_ENC         , MUL_S_PH_DESC   <NanoMipsRO_TYPES>;

def PRECRQ_PH_W_NM      : PRECRQ_PH_W_NM_ENC     , PRECRQ_PH_W_DESC      <NanoMipsRO_TYPES>;
def PRECR_QB_PH_NM      : PRECR_QB_PH_NM_ENC     , PRECR_QB_PH_DESC      <NanoMipsRO_TYPES>;
def PRECR_SRA_PH_W_NM   : PRECR_SRA_PH_W_NM_ENC  , PRECR_SRA_PH_W_DESC   <NanoMipsRO_TYPES>;
def PRECR_SRA_R_PH_W_NM : PRECR_SRA_R_PH_W_NM_ENC, PRECR_SRA_R_PH_W_DESC <NanoMipsRO_TYPES>;
def PRECRQ_QB_PH_NM     : PRECRQ_QB_PH_NM_ENC    , PRECRQ_QB_PH_DESC     <NanoMipsRO_TYPES>;
def PRECRQU_S_QB_PH_NM  : PRECRQU_S_QB_PH_NM_ENC , PRECRQU_S_QB_PH_DESC  <NanoMipsRO_TYPES>;
def PRECRQ_RS_PH_W_NM   : PRECRQ_RS_PH_W_NM_ENC  , PRECRQ_RS_PH_W_DESC   <NanoMipsRO_TYPES>;

def SHLL_QB_NM     : SHLL_QB_NM_ENC          , SHLL_QB_NM_DESC;
def SHRL_QB_NM     : SHRL_QB_NM_ENC          , SHRL_QB_NM_DESC;
def SHRL_PH_NM     : SHRL_PH_NM_ENC          , SHRL_PH_NM_DESC;
def SHLL_PH_NM     : SHLL_PH_NM_ENC          , SHLL_PH_NM_DESC;
def SHLL_S_PH_NM   : SHLL_S_PH_NM_ENC        , SHLL_S_PH_NM_DESC;
def SHLL_S_W_NM    : SHLL_S_W_NM_ENC         , SHLL_S_W_DESC   <NanoMipsRO_TYPES>;
def SHLLV_S_W_NM   : SHLLV_S_W_NM_ENC        , SHLLV_S_W_DESC  <NanoMipsRO_TYPES>;
def SHLLV_QB_NM    : SHLLV_QB_NM_ENC         , SHLLV_QB_DESC   <NanoMipsRO_TYPES>;
def SHLLV_PH_NM    : SHLLV_PH_NM_ENC         , SHLLV_PH_DESC   <NanoMipsRO_TYPES>;
def SHLLV_S_PH_NM  : SHLLV_S_PH_NM_ENC       , SHLLV_S_PH_DESC <NanoMipsRO_TYPES>;
def SHRLV_PH_NM    : SHRLV_PH_NM_ENC         , SHRLV_PH_DESC   <NanoMipsRO_TYPES>;
def SHRLV_QB_NM    : SHRLV_QB_NM_ENC         , SHRLV_QB_DESC   <NanoMipsRO_TYPES>;
def SHRA_QB_NM     : SHRA_QB_NM_ENC          , SHRA_QB_DESC    <NanoMipsRO_TYPES>;
def SHRA_R_QB_NM   : SHRA_R_QB_NM_ENC        , SHRA_R_QB_DESC  <NanoMipsRO_TYPES>;
def SHRA_PH_NM     : SHRA_PH_NM_ENC          , SHRA_PH_DESC    <NanoMipsRO_TYPES>;
def SHRA_R_PH_NM   : SHRA_R_PH_NM_ENC        , SHRA_R_PH_DESC  <NanoMipsRO_TYPES>;
def SHRA_R_W_NM    : SHRA_R_W_NM_ENC         , SHRA_R_W_DESC   <NanoMipsRO_TYPES>;
def SHRAV_QB_NM    : SHRAV_QB_NM_ENC         , SHRAV_QB_DESC   <NanoMipsRO_TYPES>;
def SHRAV_R_QB_NM  : SHRAV_R_QB_NM_ENC       , SHRAV_R_QB_DESC <NanoMipsRO_TYPES>;
def SHRAV_PH_NM    : SHRAV_PH_NM_ENC         , SHRAV_PH_DESC   <NanoMipsRO_TYPES>;
def SHRAV_R_PH_NM  : SHRAV_R_PH_NM_ENC       , SHRAV_R_PH_DESC <NanoMipsRO_TYPES>;
def SHRAV_R_W_NM   : SHRAV_R_W_NM_ENC        , SHRAV_R_W_DESC  <NanoMipsRO_TYPES>;
def APPEND_NM      : APPEND_NM_ENC           , APPEND_DESC     <NanoMipsRO_TYPES>;
def MFHI_NM        : MFHI_NM_ENC             , MFHI_NM_DESC    <NanoMipsRO_TYPES>;
def MFLO_NM        : MFLO_NM_ENC             , MFLO_NM_DESC    <NanoMipsRO_TYPES>;
def MTHI_NM        : MTHI_NM_ENC             , MTHI_DESC       <NanoMipsRO_TYPES>;
def MTLO_NM        : MTLO_NM_ENC             , MTLO_DESC       <NanoMipsRO_TYPES>;

def MTHLIP_NM      : MTHLIP_NM_ENC           , MTHLIP_DESC     <NanoMipsRO_TYPES>;
def SHILOV_NM      : SHILOV_NM_ENC           , SHILOV_DESC     <NanoMipsRO_TYPES>;
def SHILO_NM       : SHILO_NM_ENC            , SHILO_DESC      <NanoMipsRO_TYPES>;
def BITREV_NM      : BITREV_NM_ENC           , BITREV_DESC     <NanoMipsRO_TYPES>;
def INSV_NM        : INSV_NM_ENC             , INSV_DESC       <NanoMipsRO_TYPES>;
def PACKRL_PH_NM   : PACKRL_PH_NM_ENC        , PACKRL_PH_DESC  <NanoMipsRO_TYPES>;

def MULT_DSP_NM    : MADD_DSP_NM_ENC<0b00110>, MULT_DSP_DESC   <NanoMipsRO_TYPES>;
def MULTU_DSP_NM   : MADD_DSP_NM_ENC<0b01110>, MULTU_DSP_DESC  <NanoMipsRO_TYPES>;
def MADD_DSP_NM    : MADD_DSP_NM_ENC<0b00101>, MADD_DSP_DESC   <NanoMipsRO_TYPES>;
def MADDU_DSP_NM   : MADD_DSP_NM_ENC<0b01101>, MADDU_DSP_DESC  <NanoMipsRO_TYPES>;
def MSUB_DSP_NM    : MADD_DSP_NM_ENC<0b10101>, MSUB_DSP_DESC   <NanoMipsRO_TYPES>;
def MSUBU_DSP_NM   : MADD_DSP_NM_ENC<0b11101>, MSUBU_DSP_DESC  <NanoMipsRO_TYPES>;

def CMPGDU_EQ_QB_NM: CMPGDU_EQ_QB_NM_ENC     , CMPGDU_EQ_QB_DESC<NanoMipsRO_TYPES>;
def CMPGDU_LE_QB_NM: CMPGDU_LE_QB_NM_ENC     , CMPGDU_LE_QB_DESC<NanoMipsRO_TYPES>;
def CMPGDU_LT_QB_NM: CMPGDU_LT_QB_NM_ENC     , CMPGDU_LT_QB_DESC<NanoMipsRO_TYPES>;
def CMPGU_EQ_QB_NM : CMPGU_EQ_QB_NM_ENC      , CMPGU_EQ_QB_DESC<NanoMipsRO_TYPES>;
def CMPGU_LT_QB_NM : CMPGU_LT_QB_NM_ENC      , CMPGU_LT_QB_DESC<NanoMipsRO_TYPES>;
def CMPGU_LE_QB_NM : CMPGU_LE_QB_NM_ENC      , CMPGU_LE_QB_DESC<NanoMipsRO_TYPES>;
def CMP_EQ_PH_NM   : CMP_EQ_PH_NM_ENC        , CMP_EQ_PH_DESC  <NanoMipsRO_TYPES>;
def CMP_LE_PH_NM   : CMP_LE_PH_NM_ENC        , CMP_LE_PH_DESC  <NanoMipsRO_TYPES>;
def CMP_LT_PH_NM   : CMP_LT_PH_NM_ENC        , CMP_LT_PH_DESC  <NanoMipsRO_TYPES>;
def CMPU_EQ_QB_NM  : CMPU_EQ_QB_NM_ENC       , CMPU_EQ_QB_DESC <NanoMipsRO_TYPES>;
def CMPU_LE_QB_NM  : CMPU_LE_QB_NM_ENC       , CMPU_LE_QB_DESC <NanoMipsRO_TYPES>;
def CMPU_LT_QB_NM  : CMPU_LT_QB_NM_ENC       , CMPU_LT_QB_DESC <NanoMipsRO_TYPES>;

def PICK_QB_NM     : PICK_QB_NM_ENC          , PICK_QB_DESC    <NanoMipsRO_TYPES>;
def PICK_PH_NM     : PICK_PH_NM_ENC          , PICK_PH_DESC    <NanoMipsRO_TYPES>;
def EXTPV_NM       : EXTPV_NM_ENC            , EXTPV_DESC      <NanoMipsRO_TYPES>;
def EXTP_NM        : EXTP_NM_ENC             , EXTP_DESC       <NanoMipsRO_TYPES>;
def EXTPDPV_NM     : EXTPDPV_NM_ENC          , EXTPDPV_DESC    <NanoMipsRO_TYPES>;
def EXTPDP_NM      : EXTPDP_NM_ENC           , EXTPDP_DESC     <NanoMipsRO_TYPES>;
def EXTRV_W_NM     : EXTRV_W_NM_ENC          , EXTRV_W_DESC    <NanoMipsRO_TYPES>;
def EXTR_W_NM      : EXTR_W_NM_ENC           , EXTR_W_DESC     <NanoMipsRO_TYPES>;
def EXTRV_R_W_NM   : EXTRV_R_W_NM_ENC        , EXTRV_R_W_DESC  <NanoMipsRO_TYPES>;
def EXTR_R_W_NM    : EXTR_R_W_NM_ENC         , EXTR_R_W_DESC   <NanoMipsRO_TYPES>;
def EXTRV_RS_W_NM  : EXTRV_RS_W_NM_ENC       , EXTRV_RS_W_DESC <NanoMipsRO_TYPES>;
def EXTR_RS_W_NM   : EXTR_RS_W_NM_ENC        , EXTR_RS_W_DESC  <NanoMipsRO_TYPES>;
def EXTR_S_H_NM    : EXTR_S_H_NM_ENC         , EXTR_S_H_DESC   <NanoMipsRO_TYPES>;
def EXTRV_S_H_NM   : EXTRV_S_H_NM_ENC        , EXTRV_S_H_DESC  <NanoMipsRO_TYPES>;
def RADDU_W_QB_NM  : RADDU_W_QB_NM_ENC       , RADDU_W_QB_DESC <NanoMipsRO_TYPES>;
def REPLV_PH_NM    : REPLV_PH_NM_ENC         , REPLV_PH_DESC   <NanoMipsRO_TYPES>;
def REPLV_QB_NM    : REPLV_QB_NM_ENC         , REPLV_QB_DESC   <NanoMipsRO_TYPES>;
def REPL_PH_NM     : REPL_PH_NM_ENC          , REPL_PH_DESC    <NanoMipsRO_TYPES>;
def REPL_QB_NM     : REPL_QB_NM_ENC          , REPL_QB_DESC    <NanoMipsRO_TYPES>;

def RDDSP_NM       : RDDSP_NM_ENC            , RDDSP_NM_DESC;
def WRDSP_NM       : WRDSP_NM_ENC            , WRDSP_NM_DESC;

def BPOSGE32C_NM   : BPOSGE32C_NM_ENC        , BPOSGE32C_MMR3_DESC<brtarget14_nm>;

// Pseudos.
let AdditionalPredicates = [HasDSPNanoMips] in {
def BPOSGE32_PSEUDO_NM :
  BPOSGE32_PSEUDO_DESC_BASE<int_mips_bposge32, NanoMipsRO_TYPES>,
  Uses<[DSPPos]>;
}

let AdditionalPredicates = [HasDSPNanoMips, HasDSP] in {
  def PseudoMTLOHI_DSP_NM : PseudoMTLOHI<ACC64DSPNM, GPRNM32>;
}

def : BitconvertPat<i32,   v2i16, GPRNM32, DSPRNM,  HasDSPNanoMips>;
def : BitconvertPat<v2i16, i32,   DSPRNM,  GPRNM32, HasDSPNanoMips>;
def : BitconvertPat<i32,   v4i8,  GPRNM32, DSPRNM,  HasDSPNanoMips>;
def : BitconvertPat<v4i8,  i32,   DSPRNM,  GPRNM32, HasDSPNanoMips>;
def : BitconvertPat<v4i8, v2i16, DSPR, DSPR, HasDSPNanoMips>;
def : BitconvertPat<v2i16, v4i8, DSPR, DSPR, HasDSPNanoMips>;

def : DSPPat<(v2i16 (load addruimm12:$a)),
             (v2i16 (COPY_TO_REGCLASS (LW_NM addruimm12:$a), DSPRNM)), HasDSPNanoMips>;
def : DSPPat<(v2i16 (load GPRNM32:$a)),
             (v2i16 (COPY_TO_REGCLASS (LW_NM GPRNM32:$a, 0), DSPRNM)), HasDSPNanoMips>;
def : DSPPat<(v4i8 (load addruimm12:$a)),
	     (v4i8 (COPY_TO_REGCLASS (LW_NM addruimm12:$a), DSPRNM)), HasDSPNanoMips>;
def : DSPPat<(v4i8 (load GPRNM32:$a)),
	     (v4i8 (COPY_TO_REGCLASS (LW_NM GPRNM32:$a, 0), DSPRNM)), HasDSPNanoMips>;
def : DSPPat<(store (v2i16 DSPRNM:$val), addruimm12:$a),
	     (SW_NM (COPY_TO_REGCLASS DSPRNM:$val, GPRNM32), addruimm12:$a), HasDSPNanoMips>;
def : DSPPat<(store (v2i16 DSPRNM:$val), GPRNM32:$a),
	     (SW_NM (COPY_TO_REGCLASS DSPRNM:$val, GPRNM32), GPRNM32:$a, 0), HasDSPNanoMips>;
def : DSPPat<(store (v4i8 DSPRNM:$val), addruimm12:$a),
	     (SW_NM (COPY_TO_REGCLASS DSPRNM:$val, GPRNM32), addruimm12:$a), HasDSPNanoMips>;
def : DSPPat<(store (v4i8 DSPRNM:$val), GPRNM32:$a),
	     (SW_NM (COPY_TO_REGCLASS DSPRNM:$val, GPRNM32), GPRNM32:$a, 0), HasDSPNanoMips>;

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
def : DSPBinPat<ADDSC_NM  ,  i32,  int_mips_addsc  , HasDSPNanoMips>;
def : DSPBinPat<ADDSC_NM  ,  i32,  addc            , HasDSPNanoMips>;
def : DSPBinPat<ADDWC_NM  ,  i32,  int_mips_addwc  , HasDSPNanoMips>;
def : DSPBinPat<ADDWC_NM  ,  i32,  adde            , HasDSPNanoMips>;

def : DSPShiftPat<SHLL_PH_NM, v2i16, MipsSHLL_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHRA_PH_NM, v2i16, MipsSHRA_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHRL_PH_NM, v2i16, MipsSHRL_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHLL_PH_NM, v2i16, int_mips_shll_ph,  immZExt4, HasDSPNanoMips>;
def : DSPShiftPat<SHRA_PH_NM, v2i16, int_mips_shra_ph,  immZExt4, HasDSPNanoMips>;
def : DSPShiftPat<SHRL_PH_NM, v2i16, int_mips_shrl_ph,  immZExt4, HasDSPNanoMips>;
def : DSPShiftPat<SHLL_QB_NM, v4i8,  MipsSHLL_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHRA_QB_NM, v4i8,  MipsSHRA_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHRL_QB_NM, v4i8,  MipsSHRL_DSP,      imm,      HasDSPNanoMips>;
def : DSPShiftPat<SHLL_QB_NM, v4i8,  int_mips_shll_qb,  immZExt3, HasDSPNanoMips>;
def : DSPShiftPat<SHRA_QB_NM, v4i8,  int_mips_shra_qb,  immZExt3, HasDSPNanoMips>;
def : DSPShiftPat<SHRL_QB_NM, v4i8,  int_mips_shrl_qb,  immZExt3, HasDSPNanoMips>;

def : EXTR_W_TY1_R1_Pat<MipsEXTP,       EXTP_NM,       HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R2_Pat<MipsEXTP,       EXTPV_NM,      HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R1_Pat<MipsEXTPDP,     EXTPDP_NM,     HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R2_Pat<MipsEXTPDP,     EXTPDPV_NM,    HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R1_Pat<MipsEXTR_W,     EXTR_W_NM,     HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R2_Pat<MipsEXTR_W,     EXTRV_W_NM,    HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R1_Pat<MipsEXTR_R_W,   EXTR_R_W_NM,   HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R2_Pat<MipsEXTR_R_W,   EXTRV_R_W_NM,  HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R1_Pat<MipsEXTR_RS_W,  EXTR_RS_W_NM,  HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R2_Pat<MipsEXTR_RS_W,  EXTRV_RS_W_NM, HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R1_Pat<MipsEXTR_S_H,   EXTR_S_H_NM,   HasDSPNanoMips, NanoMipsRO_TYPES>;
def : EXTR_W_TY1_R2_Pat<MipsEXTR_S_H,   EXTRV_S_H_NM,  HasDSPNanoMips, NanoMipsRO_TYPES>;

let isPseudo = 1, isCodeGenOnly = 1, hasNoSchedulingInfo = 1 in {
  // Pseudo instructions for loading and storing accumulator registers.
  def LOAD_ACC64DSPNM  : Load<"", ACC64DSPNMOpnd>;
  def STORE_ACC64DSPNM : Store<"", ACC64DSPNMOpnd>;

  // Pseudos for loading and storing ccond field of DSP control register.
//  def LOAD_CCOND_DSP  : Load<"load_ccond_dsp", DSPCC>;
//  def STORE_CCOND_DSP : Store<"store_ccond_dsp", DSPCC>;
}

let DecoderNamespace = "MipsDSP", Arch = "dsp",
    ASEPredicate = [HasDSP] in {
  def LWDSP_NM : Load<"lw", DSPRNMOpnd, null_frag, II_LW>,
	      LSMem_U12_Enc<0b1000>;
  def SWDSP_NM : Store<"sw", DSPRNMOpnd, null_frag, II_SW>,
	      LSMem_U12_Enc<0b1001>;
}

def PseudoCMP_EQ_PH_NM  : PseudoCMP<CMP_EQ_PH_NM,  NanoMipsRO_TYPES>;
def PseudoCMP_LT_PH_NM  : PseudoCMP<CMP_LT_PH_NM,  NanoMipsRO_TYPES>;
def PseudoCMP_LE_PH_NM  : PseudoCMP<CMP_LE_PH_NM,  NanoMipsRO_TYPES>;
def PseudoCMPU_EQ_QB_NM : PseudoCMP<CMPU_EQ_QB_NM, NanoMipsRO_TYPES>;
def PseudoCMPU_LT_QB_NM : PseudoCMP<CMPU_LT_QB_NM, NanoMipsRO_TYPES>;
def PseudoCMPU_LE_QB_NM : PseudoCMP<CMPU_LE_QB_NM, NanoMipsRO_TYPES>;

def PseudoPICK_PH_NM : PseudoPICK<PICK_PH_NM, NanoMipsRO_TYPES>;
def PseudoPICK_QB_NM : PseudoPICK<PICK_QB_NM, NanoMipsRO_TYPES>;

// SETCC/SELECT_CC patterns.
// Copies of DSPSetCCPat, DSPSetCCPatInv
// Could not parametrize the operands since DSPR and DSPRNM
// do not belong to the same type
class DSPSetCCPatNM<Instruction Cmp, Instruction Pick, ValueType ValTy,
                 CondCode CC, Predicate Pred> :
  DSPPat<(ValTy (MipsSETCC_DSP ValTy:$a, ValTy:$b, CC)),
         (ValTy (Pick (ValTy (Cmp ValTy:$a, ValTy:$b)),
                     (ValTy (COPY_TO_REGCLASS (ADDIU_NM ZERO_NM, -1), DSPRNM)),
                     (ValTy ZERO_NM))), Pred>;

class DSPSetCCPatInvNM<Instruction Cmp, Instruction Pick, ValueType ValTy,
                    CondCode CC, Predicate Pred> :
  DSPPat<(ValTy (MipsSETCC_DSP ValTy:$a, ValTy:$b, CC)),
         (ValTy (Pick (ValTy (Cmp ValTy:$a, ValTy:$b)),
                     (ValTy ZERO_NM),
                     (ValTy (COPY_TO_REGCLASS (ADDIU_NM ZERO_NM, -1), DSPRNM)))),
         Pred>;

def : DSPSetCCPatNM<PseudoCMP_EQ_PH_NM,     PseudoPICK_PH_NM, v2i16, SETEQ,  HasDSPNanoMips>;
def : DSPSetCCPatNM<PseudoCMP_LT_PH_NM,     PseudoPICK_PH_NM, v2i16, SETLT,  HasDSPNanoMips>;
def : DSPSetCCPatNM<PseudoCMP_LE_PH_NM,     PseudoPICK_PH_NM, v2i16, SETLE,  HasDSPNanoMips>;
def : DSPSetCCPatInvNM<PseudoCMP_EQ_PH_NM,  PseudoPICK_PH_NM, v2i16, SETNE,  HasDSPNanoMips>;
def : DSPSetCCPatInvNM<PseudoCMP_LT_PH_NM,  PseudoPICK_PH_NM, v2i16, SETGE,  HasDSPNanoMips>;
def : DSPSetCCPatInvNM<PseudoCMP_LE_PH_NM,  PseudoPICK_PH_NM, v2i16, SETGT,  HasDSPNanoMips>;
def : DSPSetCCPatNM<PseudoCMPU_EQ_QB_NM,    PseudoPICK_QB_NM, v4i8,  SETEQ,  HasDSPNanoMips>;
def : DSPSetCCPatNM<PseudoCMPU_LT_QB_NM,    PseudoPICK_QB_NM, v4i8,  SETULT, HasDSPNanoMips>;
def : DSPSetCCPatNM<PseudoCMPU_LE_QB_NM,    PseudoPICK_QB_NM, v4i8,  SETULE, HasDSPNanoMips>;
def : DSPSetCCPatInvNM<PseudoCMPU_EQ_QB_NM, PseudoPICK_QB_NM, v4i8,  SETNE,  HasDSPNanoMips>;
def : DSPSetCCPatInvNM<PseudoCMPU_LT_QB_NM, PseudoPICK_QB_NM, v4i8,  SETUGE, HasDSPNanoMips>;
def : DSPSetCCPatInvNM<PseudoCMPU_LE_QB_NM, PseudoPICK_QB_NM, v4i8,  SETUGT, HasDSPNanoMips>;

def : DSPSelectCCPat<PseudoCMP_EQ_PH_NM,     PseudoPICK_PH_NM, v2i16, SETEQ , HasDSPNanoMips>;
def : DSPSelectCCPat<PseudoCMP_LT_PH_NM,     PseudoPICK_PH_NM, v2i16, SETLT , HasDSPNanoMips>;
def : DSPSelectCCPat<PseudoCMP_LE_PH_NM,     PseudoPICK_PH_NM, v2i16, SETLE , HasDSPNanoMips>;
def : DSPSelectCCPatInv<PseudoCMP_EQ_PH_NM,  PseudoPICK_PH_NM, v2i16, SETNE , HasDSPNanoMips>;
def : DSPSelectCCPatInv<PseudoCMP_LT_PH_NM,  PseudoPICK_PH_NM, v2i16, SETGE , HasDSPNanoMips>;
def : DSPSelectCCPatInv<PseudoCMP_LE_PH_NM,  PseudoPICK_PH_NM, v2i16, SETGT , HasDSPNanoMips>;
def : DSPSelectCCPat<PseudoCMPU_EQ_QB_NM,    PseudoPICK_QB_NM, v4i8,  SETEQ , HasDSPNanoMips>;
def : DSPSelectCCPat<PseudoCMPU_LT_QB_NM,    PseudoPICK_QB_NM, v4i8,  SETULT, HasDSPNanoMips>;
def : DSPSelectCCPat<PseudoCMPU_LE_QB_NM,    PseudoPICK_QB_NM, v4i8,  SETULE, HasDSPNanoMips>;
def : DSPSelectCCPatInv<PseudoCMPU_EQ_QB_NM, PseudoPICK_QB_NM, v4i8,  SETNE , HasDSPNanoMips>;
def : DSPSelectCCPatInv<PseudoCMPU_LT_QB_NM, PseudoPICK_QB_NM, v4i8,  SETUGE, HasDSPNanoMips>;
def : DSPSelectCCPatInv<PseudoCMPU_LE_QB_NM, PseudoPICK_QB_NM, v4i8,  SETUGT, HasDSPNanoMips>;

let AdditionalPredicates = [HasDSPNanoMips] in {
  def : NanoMipsInstAlias<"rddsp $rt", (RDDSP_NM GPRNM32Opnd:$rt, 0b1111111)>;
  def : NanoMipsInstAlias<"wrdsp $rt", (WRDSP_NM GPRNM32Opnd:$rt, 0b1111111)>;
}