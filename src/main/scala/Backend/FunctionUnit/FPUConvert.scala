import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import ZirconConfig.FloatConfig._
import fudian._

/**
 * FPU类型转换模块 - 封装IntToFP和FPToInt
 * convertType = 0: IntToFP (FCVT.S.W, FCVT.S.WU)
 * convertType = 1: FPToInt (FCVT.W.S, FCVT.WU.S)
 */
class FPUConvertIO extends Bundle {
    val rs1Data = Input(UInt(32.W))
    val op = Input(UInt(7.W))
    val rm = Input(UInt(3.W))
    val res = Output(UInt(32.W))
    val fflags = Output(UInt(5.W))
}

class FPUConvert(val convertType: Int) extends Module {
    val io = IO(new FPUConvertIO)
    
    if (convertType == 0) {
        // IntToFP转换 (FCVT.S.W, FCVT.S.WU)
        val int_to_fp = Module(new IntToFP(expWidth, precision))
        
        // 判断是有符号还是无符号转换
        val is_unsigned = io.op === FCVT_S_WU
        
        // 连接输入
        int_to_fp.io.int := Cat(0.U(32.W), io.rs1Data)  // 扩展到64位
        int_to_fp.io.sign := !is_unsigned               // true表示有符号
        int_to_fp.io.long := false.B                    // 32位转换
        int_to_fp.io.rm := io.rm
        
        // 连接输出
        io.res := int_to_fp.io.result
        io.fflags := int_to_fp.io.fflags
        
    } else if (convertType == 1) {
        // FPToInt转换 (FCVT.W.S, FCVT.WU.S)
        val fp_to_int = Module(new FPToInt(expWidth, precision))
        
        // 判断是有符号还是无符号转换
        val is_unsigned = io.op === FCVT_WU_S
        
        // 连接输入
        fp_to_int.io.a := io.rs1Data
        fp_to_int.io.rm := io.rm
        // op编码: [1:0] => {long, signed}
        // FCVT.W.S: 32位有符号 => op=01 (0b01)
        // FCVT.WU.S: 32位无符号 => op=00 (0b00)
        fp_to_int.io.op := Cat(false.B, !is_unsigned)  // [1]=0 (32位), [0]=signed
        
        // 连接输出（取低32位）
        io.res := fp_to_int.io.result(31, 0)
        io.fflags := fp_to_int.io.fflags
        
    } else {
        // 不应该到达这里
        io.res := 0.U
        io.fflags := 0.U
    }
}

