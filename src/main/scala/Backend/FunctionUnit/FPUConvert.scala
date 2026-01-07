import chisel3._
import chisel3.util._
import fudian._

// FPU 类型转换模块 IO 接口
class FPUConvertIO extends Bundle {
    val rs1Data = Input(UInt(32.W))
    val op      = Input(UInt(7.W))
    val rm      = Input(UInt(3.W))
    val res     = Output(UInt(32.W))
    val fflags  = Output(UInt(5.W))
}

// FPU 类型转换模块
// convertType: 1 = FPToInt, 0 = IntToFP
class FPUConvert(val convertType: Int) extends Module {
    val io = IO(new FPUConvertIO)
    
    // 使用统一的浮点参数配置
    val expWidth = ZirconConfig.FloatConfig.expWidth
    val precision = ZirconConfig.FloatConfig.precision
    
    val op = io.op
    
    if (convertType == 1) {
        // FPToInt: 浮点转整数
        // FCVT.W.S  (0x58): 浮点转有符号整数
        // FCVT.WU.S (0x59): 浮点转无符号整数
        val isFCVT_W_S  = op === ZirconConfig.EXEOp.FCVT_W_S
        val isFCVT_WU_S = op === ZirconConfig.EXEOp.FCVT_WU_S
        val isConvert = isFCVT_W_S || isFCVT_WU_S
        
        val fpToInt = Module(new FPToInt(expWidth, precision))
        fpToInt.io.a := io.rs1Data
        fpToInt.io.rm := io.rm
        // op: 00 = wu, 01 = w, 10 = lu, 11 = l (只支持32位)
        fpToInt.io.op := Cat(0.U(1.W), isFCVT_W_S)  // bit0: signed
        
        io.res := Mux(isConvert, fpToInt.io.result(31, 0), io.rs1Data)
        io.fflags := Mux(isConvert, fpToInt.io.fflags, 0.U)
    } else {
        // IntToFP: 整数转浮点
        // FCVT.S.W  (0x5A): 有符号整数转浮点
        // FCVT.S.WU (0x5B): 无符号整数转浮点
        val isFCVT_S_W  = op === ZirconConfig.EXEOp.FCVT_S_W
        val isFCVT_S_WU = op === ZirconConfig.EXEOp.FCVT_S_WU
        val isConvert = isFCVT_S_W || isFCVT_S_WU
        
        val intToFP = Module(new IntToFP(expWidth, precision))
        intToFP.io.int := Cat(0.U(32.W), io.rs1Data)  // 扩展到64位
        intToFP.io.sign := isFCVT_S_W  // 是否有符号
        intToFP.io.long := false.B     // 32位整数
        intToFP.io.rm := io.rm
        
        io.res := Mux(isConvert, intToFP.io.result, io.rs1Data)
        io.fflags := Mux(isConvert, intToFP.io.fflags, 0.U)
    }
}

