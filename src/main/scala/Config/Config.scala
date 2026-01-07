package ZirconConfig
import chisel3._
import chisel3.util._

object EXEOp {
    // alu
    val ADD     = 0x00.U(7.W)
    val SLL     = 0x01.U(7.W)
    val SLT     = 0x02.U(7.W)
    val SLTU    = 0x03.U(7.W)
    val XOR     = 0x04.U(7.W)
    val SRL     = 0x05.U(7.W)
    val OR      = 0x06.U(7.W)
    val AND     = 0x07.U(7.W)
    val SUB     = 0x08.U(7.W)
    val LUI     = 0x09.U(7.W)
    val SRA     = 0x0d.U(7.W)
    
    // branch
    val BEQ     = 0x18.U(7.W)
    val BNE     = 0x19.U(7.W)
    val JALR    = 0x1a.U(7.W)
    val JAL     = 0x1b.U(7.W)
    val BLT     = 0x1c.U(7.W)
    val BGE     = 0x1d.U(7.W)
    val BLTU    = 0x1e.U(7.W)
    val BGEU    = 0x1f.U(7.W)
    
    // mul and div
    val MUL     = 0x10.U(7.W)
    val MULH    = 0x11.U(7.W)
    val MULHSU  = 0x12.U(7.W)
    val MULHU   = 0x13.U(7.W)
    val DIV     = 0x14.U(7.W)
    val DIVU    = 0x15.U(7.W)
    val REM     = 0x16.U(7.W)
    val REMU    = 0x17.U(7.W)

    // mem
    val LB      = 0x10.U(7.W)
    val LH      = 0x11.U(7.W)
    val LW      = 0x12.U(7.W)
    val LBU     = 0x14.U(7.W)
    val FLW     = 0x54.U(7.W)
    val LHU     = 0x15.U(7.W)
    val SB      = 0x20.U(7.W)
    val SH      = 0x21.U(7.W)
    val SW      = 0x22.U(7.W)
    val FSW     = 0x62.U(7.W)


    // fpu
    // 对于 OP-FP (opcode=0x53) 类指令：使用 funct7 和 funct3 的组合
    // 基本算术运算 (funct7[4:2])
    val FADD_S   = 0x40.U(7.W)  // funct7=0000000 -> inst[29:27]=000, inst[14:12]=rm
    val FSUB_S   = 0x41.U(7.W)  // funct7=0000100 -> inst[29:27]=001, inst[14:12]=rm
    val FMUL_S   = 0x42.U(7.W)  // funct7=0001000 -> inst[29:27]=010, inst[14:12]=rm
    val FDIV_S   = 0x43.U(7.W)  // funct7=0001100 -> inst[29:27]=011, inst[14:12]=rm
    val FSQRT_S  = 0x4B.U(7.W)  // funct7=0101100 -> inst[29:27]=101, inst[14:12]=rm, rs2=0
    // 符号注入和最值 (funct7[4:2]=100, 用 funct3 区分)
    val FSGNJ_S  = 0x44.U(7.W)  // funct7=0010000, funct3=000 -> inst[29:27]=100, inst[14:12]=000
    val FSGNJN_S = 0x45.U(7.W)  // funct7=0010000, funct3=001 -> inst[29:27]=100, inst[14:12]=001
    val FSGNJX_S = 0x46.U(7.W)  // funct7=0010000, funct3=010 -> inst[29:27]=100, inst[14:12]=010
    val FMIN_S   = 0x47.U(7.W)  // funct7=0010100, funct3=000 -> inst[29:27]=101, inst[14:12]=000
    val FMAX_S   = 0x48.U(7.W)  // funct7=0010100, funct3=001 -> inst[29:27]=101, inst[14:12]=001
    // 比较运算 (funct7=1010000, 用 funct3 区分)
    val FLE_S    = 0x54.U(7.W)  // funct7=1010000, funct3=000 -> inst[31]=1, inst[29:27]=010, inst[14:12]=000
    val FLT_S    = 0x55.U(7.W)  // funct7=1010000, funct3=001 -> inst[31]=1, inst[29:27]=010, inst[14:12]=001
    val FEQ_S    = 0x56.U(7.W)  // funct7=1010000, funct3=010 -> inst[31]=1, inst[29:27]=010, inst[14:12]=010
    // 类型转换 - 浮点到整数 (funct7=1100000, 用 rs2[0] 区分)
    val FCVT_W_S  = 0x58.U(7.W) // funct7=1100000, rs2=00000 -> inst[31]=1, inst[29:27]=100, inst[20]=0
    val FCVT_WU_S = 0x59.U(7.W) // funct7=1100000, rs2=00001 -> inst[31]=1, inst[29:27]=100, inst[20]=1
    // 类型转换 - 整数到浮点 (funct7=1101000, 用 rs2[0] 区分)
    val FCVT_S_W  = 0x5A.U(7.W) // funct7=1101000, rs2=00000 -> inst[31]=1, inst[29:27]=101, inst[20]=0
    val FCVT_S_WU = 0x5B.U(7.W) // funct7=1101000, rs2=00001 -> inst[31]=1, inst[29:27]=101, inst[20]=1
    // 移动和分类 (funct7=111x000, 用 funct7[3] 和 funct3[0] 区分)
    val FMV_X_W   = 0x5C.U(7.W) // funct7=1110000, funct3=000 -> inst[31:28]=1110, inst[12]=0
    val FCLASS_S  = 0x5D.U(7.W) // funct7=1110000, funct3=001 -> inst[31:28]=1110, inst[12]=1
    val FMV_W_X   = 0x5E.U(7.W) // funct7=1111000, funct3=000 -> inst[31:28]=1111, inst[12]=0
    // 融合乘加运算 (用 opcode[3:2] 区分)
    val FMADD_S   = 0x50.U(7.W) // opcode=1000011 -> inst[3:2]=00
    val FMSUB_S   = 0x51.U(7.W) // opcode=1000111 -> inst[3:2]=01
    val FNMSUB_S  = 0x52.U(7.W) // opcode=1001011 -> inst[3:2]=10
    val FNMADD_S  = 0x53.U(7.W) // opcode=1001111 -> inst[3:2]=11

}

object InstructionType {
    val R_TYPE = 0x0.U(3.W)
    val I_TYPE = 0x1.U(3.W)
    val S_TYPE = 0x2.U(3.W)
    val B_TYPE = 0x3.U(3.W)
    val U_TYPE = 0x4.U(3.W)
    val J_TYPE = 0x5.U(3.W)
}
object Src1Sel {
    val RS1 = 0x0.U(1.W)
    val PC  = 0x1.U(1.W)
}
object Src2Sel {
    val RS2 = 0x0.U(1.W)
    val IMM = 0x1.U(1.W)
}
object Valid {
    val Y = true.B
    val N = false.B
}

// 单精度浮点参数 (IEEE 754 single precision)
object FloatConfig {
    val expWidth = 8       // 指数位宽
    val precision = 24     // 尾数精度 (包含隐含位)
    val fpWidth = 32       // 浮点数总位宽
    val signBit = 31       // 符号位位置
}
