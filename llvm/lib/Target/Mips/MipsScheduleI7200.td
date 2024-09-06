def MipsI7200Model : SchedMachineModel {
  int IssueWidth = 2;
  int MicroOpBufferSize = 0;
  int LoadLatency = 2;
  int MispredictPenalty = 8;

  let CompleteModel = 1;
  let FullInstRWOverlapCheck = 1;

  // Disable all except nanomips instructions.
  list<Predicate> UnsupportedFeatures = [
    HasMips2, HasMips3_32, HasMips3_32r2, HasMips3, HasMips4_32, HasMips4_32r2,
    HasMips32, HasMips32r2, HasMips32r5, HasMips32r6,
    HasMips64, HasMips64r2, HasMips64r5, HasMips64r6,
    IsGP64bit, IsFP64bit, IsPTR64bit, IsNotSoftFloat,
    InMicroMips, InMips16Mode, HasCnMips, HasCnMipsP,
    HasDSP, HasDSPR2, HasMips3D, HasMT,
    HasCRC, HasStdEnc, HasMSA
  ];
}

// Define processor resources.
let SchedModel = MipsI7200Model, BufferSize = 0 in {
  def i7200GpMulDiv : ProcResource<1>;

  def i7200Agen : ProcResource<1>;
  def i7200Alu1 : ProcResource<1>;
  def i7200Lsu : ProcResource<1>;

  def i7200Control : ProcResource<1>;
  def i7200Ctu : ProcResource<1>;
  def i7200Alu0 : ProcResource<1>;

  def i7200Alu : ProcResGroup<[i7200Alu0, i7200Alu1]>;
}

// Define read classes for ReadAdvance.
def I7200ReadLoad : SchedRead;
def I7200ReadStore : SchedRead;
def I7200ReadSpecial : SchedRead;

// Define instruction schedules.
let SchedModel = MipsI7200Model in {

// Arithmetic and logical instructions.
def I7200WriteALU : SchedWriteRes<[i7200Alu]>;
def : InstRW<[I7200WriteALU], (instrs
  ADD_NM, ADDIU_NM, ADDIU48_NM, ADDIUGP48_NM, ADDIUGPB_NM, ADDIUGPW_NM, ADDIUNEG_NM, ADDIUR1SP_NM, ADDIUR2_NM, ADDIURS5_NM,
  ADDu_NM, ADDu16_NM, ADDu4x4_NM, SUB_NM, SUBu_NM, SUBu16_NM,
  AND_NM, AND16_NM, ANDI_NM, ANDI16_NM, OR_NM, OR16_NM, ORI_NM, XOR_NM, XOR16_NM, XORI_NM, NOP_NM, NOP32_NM,  NOR_NM, NOT16_NM,
  SRA, SRAV, SRL, SRLV, SLL, SLL16_NM, SLLV, SRA_NM, SRAV_NM, SRL_NM, SRL16_NM, SRLV_NM, SLL_NM, SLLV_NM, 
  SLT, SLTi, SLTiu, SLTu, SLT_NM, SLTI_NM, SLTIU_NM, SLTU_NM, SEQI_NM,
  SEB_NM, SEH_NM, SOV_NM,
  BITREVW_NM, BYTEREVW_NM,
  MOVE_NM, MOVN_NM, MOVZ_NM,
  CLZ_NM, CLZ, CLO_NM, CLO,
  EXT_NM, EXTW_NM, INS_NM,
  LSA_NM, SAVE_NM, SAVE16_NM, RESTORE_NM,
  ALUIPC_NM, ROTR_NM, ROTRV_NM, ROTX_NM,
  LI16_NM, LI48_NM, LUI_NM,
  LAPC32_NM, LAPC48_NM
)>;

// Movep instruction.
def I7200WriteMovep : SchedWriteRes<[i7200Control, i7200Agen, i7200Alu0, i7200Alu1]> {
  let Latency = 1;
}
def : InstRW<[I7200WriteMovep], (instrs MOVEP_NM, MOVEPREV_NM)>;

// Mul instructions.
def I7200WriteMul : SchedWriteRes<[i7200Agen, i7200GpMulDiv]> {
  let Latency = 5;
  let ResourceCycles = [1, 1];
}
def : InstRW<[I7200WriteMul], (instrs MUL_NM, MUL4x4_NM, MULU_NM, MUH_NM, MUHU_NM)>;

// Div instructions.
def I7200WriteDiv : SchedWriteRes<[i7200GpMulDiv]> {
  let Latency = 32;
  let ResourceCycles = [32];
}
def : InstRW<[I7200WriteDiv], (instrs DIV_NM, DIVU_NM, MOD_NM, MODU_NM)>;

// Load instructions.
def I7200WriteLoad : SchedWriteRes<[i7200Agen, i7200Lsu]> {
  let Latency = 2;
  let ResourceCycles = [1, 1];
}
def : InstRW<[I7200WriteLoad, I7200ReadLoad], (instrs
  LB_NM, LB16_NM, LBGP_NM, LBs9_NM, LBU_NM, LBU16_NM, LBUGP_NM, LBUs9_NM, LH_NM, LH16_NM, LHGP_NM, LHGP_NM, LHs9_NM,
  LHU_NM, LHU16_NM, LHUGP_NM, LHUs9_NM, LW_NM, LW16_NM, LW4x4_NM, LWGP16_NM, LWSP16_NM, LWs9_NM,
  LBX_NM, LBUX_NM, LWX_NM, LWXS_NM, LWXS16_NM, LHX_NM, LHXS_NM, LHUX_NM, LHUXS_NM, LWM_NM,
  LWPC_NM, LWGP_NM, UALW_NM, UALH_NM, UALWM_NM
)>;

// Store instructions.
def I7200WriteStore : SchedWriteRes<[i7200Agen, i7200Lsu]> {
  let Latency = 1;
  let ResourceCycles = [1, 1];
}
def : InstRW<[I7200WriteStore, I7200ReadStore], (instrs 
  SB_NM, SB16_NM, SBGP_NM, SBs9_NM, SH_NM, SH16_NM, SHGP_NM, SHs9_NM, SW_NM, SWs9_NM, SW16_NM, SW4x4_NM, SWGP_NM, SWGP16_NM, SWSP16_NM,
  SBX_NM, SHX_NM, SHXS_NM, SWX_NM, SWXS_NM,
  SWPC_NM, SWM_NM, UASW_NM, UASH_NM, UASWM_NM
)>;

// Branch and jump instructions.
def I7200WriteBranch : SchedWriteRes<[i7200Control, i7200Ctu]> {
  let Latency = 1;
  let ResourceCycles = [1, 1];
}
def : InstRW<[I7200WriteBranch], (instrs
  BRSC_NM, BC_NM, BC16_NM, BEQC_NM, BEQC16_NM, BEQCzero_NM, BEQIC_NM, BEQZC_NM, BEQZC16_NM, BGEC_NM, BGEIC_NM, BGEIUC_NM, BGEUC_NM,
  BLTC_NM, BLTIC_NM, BLTIUC_NM, BLTUC_NM, BNEC_NM, BNEC16_NM, BNECzero_NM, BNEIC_NM, BNEZC_NM, BNEZC16_NM,
  BBNEZC_NM, BBEQZC_NM, JALRC_NM, JALRC16_NM, JALRCHB_NM, JRC_NM, RESTOREJRC_NM, RESTOREJRC16_NM, BALC_NM, BALC16_NM, BALRSC_NM, MOVEBALC_NM
)>;

// Exception instructions.
def : InstRW<[I7200WriteBranch], (instrs
  BREAK_NM, BREAK16_NM, SDBBP16_NM, SDBBP_NM, SIGRIE_NM, SYSCALL_NM, SYSCALL16_NM
)>;

// Cache instructions.
def I7200WriteCache : SchedWriteRes<[i7200Agen, i7200Lsu, i7200Control]>{
  let Latency = 2;
  let ResourceCycles = [1, 1, 1];
}
def : InstRW<[I7200WriteCache], (instrs
  CACHE_NM, PREF_NM, PREFs9_NM, SYNCI_NM, SYNCIs9_NM
)>;

// Ginvi instruction.
def I7200WriteGinvi : SchedWriteRes<[i7200Control]>{
  let Latency = 1;
  let ResourceCycles = [1];
}
def : InstRW<[I7200WriteGinvi], (instrs
  GINVI_NM
)>;

//Exception return instructions.
def : InstRW<[I7200WriteBranch], (instrs
  DERET_NM, ERET_NM, ERETNC_NM
)>;

// COP instructions.
def I7200WriteCOP : SchedWriteRes<[i7200Control]>{
  let Latency = 2;
  let ResourceCycles = [2];
}
def : InstRW<[I7200WriteCOP], (instrs
  DI_NM, EI_NM, MTC0_NM, MTC0Sel_NM, MTHC0_NM, MTHC0Sel_NM, MFC0_NM, MFC0Sel_NM, MFHC0_NM, MFHC0Sel_NM, EHB_NM, PAUSE_NM, WAIT_NM
)>;

// Special instructions.
def I7200WriteSpecial : SchedWriteRes<[i7200Alu]>{
  let Latency = 1;
  let ResourceCycles = [1];
}
def : InstRW<[I7200WriteSpecial, I7200ReadSpecial], (instrs
  RDPGPR_NM, WRPGPR_NM
)>;

// Sync instruction.
def I7200WriteSync : SchedWriteRes<[i7200Lsu]>{
  let Latency = 1;
  let ResourceCycles = [1];
}
def : InstRW<[I7200WriteSync], (instrs
  SYNC_NM
)>;

// TLB instructions.
def I7200WriteTLB : SchedWriteRes<[i7200Control]>{
  let Latency = 2;
  let ResourceCycles = [2];
}
def : InstRW<[I7200WriteTLB], (instrs
  TLBINV_NM, TLBINVF_NM, TLBP_NM, TLBR_NM, TLBWI_NM, TLBWR_NM, GINVT_NM
)>;

// Unclear how to schedule these. Use WriteALU for now.
def : InstRW<[I7200WriteALU], (instrs TEQ_NM, TNE_NM, RDHWR_NM)>;

// Pseudo instructions.
def : InstRW<[I7200WriteALU], (instrs COPY)>;
def : InstRW<[I7200WriteBranch], (instrs
  RetRA, PseudoReturn, PseudoReturnNM, PseudoIndirectBranchNM,
  TAILCALL_NM, TAILCALLREG_NM, MUSTTAILCALLREG_NM, MUSTTAILCALL_NM, JALRCPseudo, JALRHBPseudo, ERet
)>;

// These are copied from the generic model.
def I7200Atomic : ProcResource<1> { let BufferSize = 0; }
def I7200WriteAtomic : SchedWriteRes<[I7200Atomic]> { let Latency = 2; }
def : InstRW<[I7200WriteAtomic],
    (instregex "^ATOMIC_SWAP_I(8|16|32|64)_POSTRA$")>;
def : InstRW<[I7200WriteAtomic],
    (instregex "^ATOMIC_CMP_SWAP_I(8|16|32|64)_POSTRA$")>;
def : InstRW<[I7200WriteAtomic],
    (instregex "^ATOMIC_LOAD_(ADD|SUB|AND|OR|XOR|NAND|MIN|MAX|UMIN|UMAX)"
               "_I(8|16|32|64)_POSTRA$")>;


// Define bypasses.
// Load and store instructions take 1 cycle longer if their operand was
// created by a load, arithmetic, COP or move operation.
def : ReadAdvance<I7200ReadLoad, -1, [I7200WriteLoad, I7200WriteALU,
                                      I7200WriteBranch, I7200WriteMul,
                                      I7200WriteCOP, I7200WriteSpecial,
                                      I7200WriteMovep, I7200WriteDiv]>;
def : ReadAdvance<I7200ReadStore, -1, [I7200WriteLoad, I7200WriteALU,
                                      I7200WriteBranch, I7200WriteMul,
                                      I7200WriteCOP, I7200WriteSpecial,
                                      I7200WriteMovep, I7200WriteDiv]>;
def : ReadAdvance<I7200ReadSpecial, 1, [I7200WriteMul, I7200WriteDiv]>;
}