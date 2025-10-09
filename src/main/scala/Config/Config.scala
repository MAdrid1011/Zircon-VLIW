package ZirconConfig
import chisel3._
import chisel3.util._

object EXEOp {
    // alu
    val ADD     = 0x0.U(5.W)
    val SLL     = 0x1.U(5.W)
    val SLT     = 0x2.U(5.W)
    val SLTU    = 0x3.U(5.W)
    val XOR     = 0x4.U(5.W)
    val SRL     = 0x5.U(5.W)
    val OR      = 0x6.U(5.W)
    val AND     = 0x7.U(5.W)
    val SUB     = 0x8.U(5.W)
    val SRA     = 0xd.U(5.W)
    
    // branch
    val BEQ     = 0x18.U(5.W)
    val BNE     = 0x19.U(5.W)
    val JALR    = 0x1a.U(5.W)
    val JAL     = 0x1b.U(5.W)
    val BLT     = 0x1c.U(5.W)
    val BGE     = 0x1d.U(5.W)
    val BLTU    = 0x1e.U(5.W)
    val BGEU    = 0x1f.U(5.W)
    
    // mul and div
    val MUL     = 0x10.U(5.W)
    val MULH    = 0x11.U(5.W)
    val MULHSU  = 0x12.U(5.W)
    val MULHU   = 0x13.U(5.W)
    val DIV     = 0x14.U(5.W)
    val DIVU    = 0x15.U(5.W)
    val REM     = 0x16.U(5.W)
    val REMU    = 0x17.U(5.W)
}

object JumpOp{
    val NOP     = 0x0.U(2.W)
    val BR      = 0x1.U(2.W)
    val CALL    = 0x2.U(2.W)
    val RET     = 0x3.U(2.W)
}
object RegisterFile{
    val nlreg = 32
    val wlreg = log2Ceil(nlreg)
}

object Issue{
    val niq  = Fetch.nfch
}
object Fetch{
    val nfch = 8
}

object Decode{
    val ndcd = Fetch.nfch
    val wdecode = log2Ceil(ndcd)
}

object Dispatch{
    val nfuncUnit = Fetch.nfch
}

