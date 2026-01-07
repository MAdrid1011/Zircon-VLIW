import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import ZirconConfig.InstructionType._
import ZirconConfig.Src1Sel._
import ZirconConfig.Src2Sel._
import ZirconConfig.Valid._
import ZirconUtil._
object ALUDecodeMap{
    // op, immType, src1Sel, src2Sel, rdValid, rsIsFloat, rdIsFloat, instValid
    val default = List(LUI, U_TYPE, PC, IMM, N, N, N, N)
    val map = Array(
        RVISA.LUI       -> List(LUI,        U_TYPE, PC,  IMM, Y, N, N, Y),
        RVISA.AUIPC     -> List(ADD,        U_TYPE, PC,  IMM, Y, N, N, Y),
        RVISA.ADDI      -> List(ADD,        I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.SLTI      -> List(SLT,        I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.SLTIU     -> List(SLTU,       I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.XORI      -> List(XOR,        I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.ORI       -> List(OR,         I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.ANDI      -> List(AND,        I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.SLLI      -> List(SLL,        I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.SRLI      -> List(SRL,        I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.SRAI      -> List(SRA,        I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.ADD       -> List(ADD,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.SUB       -> List(SUB,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.SLL       -> List(SLL,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.SLT       -> List(SLT,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.SLTU      -> List(SLTU,       R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.XOR       -> List(XOR,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.SRL       -> List(SRL,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.SRA       -> List(SRA,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.OR        -> List(OR,         R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.AND       -> List(AND,        R_TYPE, RS1, RS2, Y, N, N, Y),
    )
}
object BranchDecodeMap{
    val default = List(BEQ, B_TYPE, PC, IMM, N, N, N, N)
    val map = Array(
        RVISA.JAL       -> List(JAL,        J_TYPE, PC,  IMM, Y, N, N, Y),
        RVISA.JALR      -> List(JALR,       I_TYPE, PC,  IMM, Y, N, N, Y),
        RVISA.BEQ       -> List(BEQ,        B_TYPE, PC,  IMM, N, N, N, Y),
        RVISA.BNE       -> List(BNE,        B_TYPE, PC,  IMM, N, N, N, Y),
        RVISA.BLT       -> List(BLT,        B_TYPE, PC,  IMM, N, N, N, Y),
        RVISA.BGE       -> List(BGE,        B_TYPE, PC,  IMM, N, N, N, Y),
        RVISA.BLTU      -> List(BLTU,       B_TYPE, PC,  IMM, N, N, N, Y),
        RVISA.BGEU      -> List(BGEU,       B_TYPE, PC,  IMM, N, N, N, Y),
    )
}
object FPUDecodeMap{
    val default = List(FADD_S, R_TYPE, RS1, RS2, N, N, N, N)
    val map = Array(
        RVISA.FMADD_S   -> List(FMADD_S,    R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FMSUB_S   -> List(FMSUB_S,    R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FNMSUB_S  -> List(FNMSUB_S,   R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FNMADD_S  -> List(FNMADD_S,   R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FADD_S    -> List(FADD_S,     R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FSUB_S    -> List(FSUB_S,     R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FMUL_S    -> List(FMUL_S,     R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FSGNJ_S   -> List(FSGNJ_S,    R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FSGNJN_S  -> List(FSGNJN_S,   R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FSGNJX_S  -> List(FSGNJX_S,   R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FMIN_S    -> List(FMIN_S,     R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FMAX_S    -> List(FMAX_S,     R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FCVT_W_S  -> List(FCVT_W_S,   R_TYPE, RS1, RS2, Y, Y, N, Y),
        RVISA.FCVT_WU_S -> List(FCVT_WU_S,  R_TYPE, RS1, RS2, Y, Y, N, Y),
        RVISA.FMV_X_W   -> List(FMV_X_W,    R_TYPE, RS1, RS2, Y, Y, N, Y),
        RVISA.FEQ_S     -> List(FEQ_S,      R_TYPE, RS1, RS2, Y, Y, N, Y),
        RVISA.FLT_S     -> List(FLT_S,      R_TYPE, RS1, RS2, Y, Y, N, Y),
        RVISA.FLE_S     -> List(FLE_S,      R_TYPE, RS1, RS2, Y, Y, N, Y),
        RVISA.FCLASS_S  -> List(FCLASS_S,   R_TYPE, RS1, RS2, Y, Y, N, Y),
        RVISA.FCVT_S_W  -> List(FCVT_S_W,   R_TYPE, RS1, RS2, Y, N, Y, Y),
        RVISA.FCVT_S_WU -> List(FCVT_S_WU,  R_TYPE, RS1, RS2, Y, N, Y, Y),
        RVISA.FMV_W_X   -> List(FMV_W_X,    R_TYPE, RS1, RS2, Y, N, Y, Y),
    )
}
object MemDecodeMap{
    val default = List(LB, I_TYPE, RS1, IMM, N, N, N, N)
    val map = Array(
        RVISA.LB        -> List(LB,         I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.LH        -> List(LH,         I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.LW        -> List(LW,         I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.LBU       -> List(LBU,        I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.LHU       -> List(LHU,        I_TYPE, RS1, IMM, Y, N, N, Y),
        RVISA.SB        -> List(SB,         S_TYPE, RS1, IMM, N, N, N, Y),
        RVISA.SH        -> List(SH,         S_TYPE, RS1, IMM, N, N, N, Y),
        RVISA.SW        -> List(SW,         S_TYPE, RS1, IMM, N, N, N, Y),
        RVISA.FLW       -> List(FLW,        I_TYPE, RS1, IMM, Y, N, Y, Y),
        RVISA.FSW       -> List(FSW,        S_TYPE, RS1, IMM, N, N, N, Y),
    )
}
object IMulDivDecodeMap{
    val default = List(MUL, R_TYPE, RS1, RS2, N, N, N, N)
    val map = Array(
        RVISA.MUL       -> List(MUL,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.MULH      -> List(MULH,       R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.MULHSU    -> List(MULHSU,     R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.MULHU     -> List(MULHU,      R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.DIV       -> List(DIV,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.DIVU      -> List(DIVU,       R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.REM       -> List(REM,        R_TYPE, RS1, RS2, Y, N, N, Y),
        RVISA.REMU      -> List(REMU,       R_TYPE, RS1, RS2, Y, N, N, Y),
    )
}
object FDivDecodeMap {
    val default = List(FDIV_S, R_TYPE, RS1, RS2, N, N, N, N)
    val map = Array(
        RVISA.FDIV_S    -> List(FDIV_S,     R_TYPE, RS1, RS2, Y, Y, Y, Y),
        RVISA.FSQRT_S   -> List(FSQRT_S,    R_TYPE, RS1, RS2, Y, Y, Y, Y),
    )

}

class DecoderIO extends Bundle{
    val instPkgIn    = Input(new InstructionPackage())
    val instPkgOut   = Output(new InstructionPackage())

}

class Decoder(ALU: Boolean, FPU: Boolean, Branch: Boolean, Mem: Boolean, IMulDiv: Boolean, FDiv: Boolean) extends Module{
    val io = IO(new DecoderIO)

    val aluSignals      = if(ALU) ListLookup(io.instPkgIn.inst, ALUDecodeMap.default, ALUDecodeMap.map) else List(LUI, U_TYPE, PC, IMM, N, N, N, N)
    val fpuSignals      = if(FPU) ListLookup(io.instPkgIn.inst, FPUDecodeMap.default, FPUDecodeMap.map) else List(FADD_S, R_TYPE, RS1, RS2, N, N, N, N)
    val branchSignals   = if(Branch) ListLookup(io.instPkgIn.inst, BranchDecodeMap.default, BranchDecodeMap.map) else List(BEQ, B_TYPE, PC, IMM, N, N, N, N)
    val memSignals      = if(Mem) ListLookup(io.instPkgIn.inst, MemDecodeMap.default, MemDecodeMap.map) else List(LB, I_TYPE, RS1, IMM, N, N, N, N)
    val iMulDivSignals  = if(IMulDiv) ListLookup(io.instPkgIn.inst, IMulDivDecodeMap.default, IMulDivDecodeMap.map) else List(MUL, R_TYPE, RS1, RS2, N, N, N, N)
    val fDivSignals     = if(FDiv) ListLookup(io.instPkgIn.inst, FDivDecodeMap.default, FDivDecodeMap.map) else List(FDIV_S, R_TYPE, RS1, RS2, N, N, N, N)

    assert(PopCount(VecInit(aluSignals(7), fpuSignals(7), branchSignals(7), memSignals(7), iMulDivSignals(7), fDivSignals(7)).asUInt) <= 1.U, "Multiple signals are active")
    
    def selectSignal(idx: Int) = MuxCase(
        aluSignals(idx),
        Array(
            aluSignals(7).asBool      -> aluSignals(idx),
            fpuSignals(7).asBool      -> fpuSignals(idx),
            branchSignals(7).asBool   -> branchSignals(idx),
            memSignals(7).asBool      -> memSignals(idx),
            iMulDivSignals(7).asBool  -> iMulDivSignals(idx),
            fDivSignals(7).asBool     -> fDivSignals(idx)
        ).toIndexedSeq
    )
    val op        = selectSignal(0)
    val immType   = selectSignal(1)
    val src1Sel   = selectSignal(2)
    val src2Sel   = selectSignal(3)
    val rdValid_raw = selectSignal(4)
    val rs1       = selectSignal(5) ## io.instPkgIn.inst(19, 15)
    val rs2       = selectSignal(5) ## io.instPkgIn.inst(24, 20)
    val rs3       = selectSignal(5) ## io.instPkgIn.inst(31, 27)
    val rd        = selectSignal(6) ## io.instPkgIn.inst(11, 7)
    
    // 如果rd是GPR（最高位为0）且rd编号为0，则rdValid为false
    val isGPR = !selectSignal(6).asBool
    val rdIsZero = io.instPkgIn.inst(11, 7) === 0.U
    val rdValid = rdValid_raw.asBool && !(isGPR && rdIsZero)

    def immGen(immType: UInt, inst: UInt) = MuxLookup(immType, 0.U(32.W))(Seq(
        U_TYPE -> (inst(31, 12) ## 0.U(12.W)),
        J_TYPE -> SE(inst(31) ## inst(19, 12) ## inst(20) ## inst(30, 21) ## 0.U(1.W), 32),  // 修复：添加inst[20]
        I_TYPE -> SE(inst(31, 20), 32),
        S_TYPE -> SE(inst(31, 25) ## inst(11, 7), 32),
        B_TYPE -> SE(inst(31) ## inst(7) ## inst(30, 25) ## inst(11, 8) ## 0.U(1.W), 32),
    ))

    // rm (rounding mode) 位于 inst[14:12]
    val rm = io.instPkgIn.inst(14, 12)

    io.instPkgOut := io.instPkgIn.IDUpdate(rs1, rs2, rs3, rd, rdValid, op, rm, immGen(immType, io.instPkgIn.inst), src1Sel, src2Sel)
}
