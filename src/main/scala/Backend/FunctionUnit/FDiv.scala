import chisel3._
import chisel3.util._
import fudian.FDIV

// FDiv IO接口
class FDivIO extends Bundle {
    val rs1Data  = Input(UInt(32.W))
    val rs2Data  = Input(UInt(32.W))
    val op       = Input(UInt(7.W))
    val rm       = Input(UInt(3.W))
    // 控制信号
    val valid    = Input(Bool())      // 输入有效
    val kill     = Input(Bool())      // 终止运算
    // 输出信号
    val res      = Output(UInt(32.W))
    val fflags   = Output(UInt(5.W))
    val busy     = Output(Bool())     // 运算中
    val outValid = Output(Bool())     // 输出有效
}

// 浮点除法单元封装
// 支持 FDIV.S 和 FSQRT.S
class FDiv extends Module {
    val io = IO(new FDivIO)
    
    // 使用统一的浮点参数配置
    val expWidth = ZirconConfig.FloatConfig.expWidth
    val precision = ZirconConfig.FloatConfig.precision
    
    // 实例化 fudian FDIV
    val fdiv = Module(new FDIV(expWidth, precision))
    
    // 操作码判断
    val isFDIV  = io.op === ZirconConfig.EXEOp.FDIV_S
    val isFSQRT = io.op === ZirconConfig.EXEOp.FSQRT_S
    val isFDivOp = isFDIV || isFSQRT
    
    // 连接输入
    fdiv.io.a := io.rs1Data
    fdiv.io.b := io.rs2Data
    fdiv.io.rm := io.rm
    
    // 控制信号
    fdiv.io.specialIO.isSqrt := isFSQRT
    fdiv.io.specialIO.in_valid := io.valid && isFDivOp
    fdiv.io.specialIO.out_ready := true.B  // 始终准备接收结果
    fdiv.io.specialIO.kill := io.kill
    
    // 输出信号
    // 注意：out_valid 置1后需多打一拍才能得到正确结果（fudian已知问题）
    val outValidDelayed = RegNext(fdiv.io.specialIO.out_valid, false.B)
    val resultReg = RegEnable(fdiv.io.result, fdiv.io.specialIO.out_valid)
    val fflagsReg = RegEnable(fdiv.io.fflags, fdiv.io.specialIO.out_valid)
    
    io.res := Mux(isFDivOp, resultReg, io.rs1Data)
    io.fflags := Mux(isFDivOp, fflagsReg, 0.U)
    io.busy := isFDivOp && !fdiv.io.specialIO.in_ready
    io.outValid := outValidDelayed
}
