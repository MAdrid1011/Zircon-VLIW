import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import ZirconConfig.FloatConfig._
import fudian._

/**
 * FDivWrapper包装类 - 封装fudian的FDIV模块
 * 支持: FDIV.S, FSQRT.S
 * 特点: 使用握手协议，计算周期不固定
 */
class FDivWrapperIO extends Bundle {
    val rs1Data = Input(UInt(32.W))
    val rs2Data = Input(UInt(32.W))
    val op = Input(UInt(7.W))
    val rm = Input(UInt(3.W))
    val valid = Input(Bool())       // 输入有效信号
    val kill = Input(Bool())        // 终止当前运算
    val res = Output(UInt(32.W))
    val fflags = Output(UInt(5.W))
    val busy = Output(Bool())       // 除法器忙信号
    val ready = Output(Bool())      // 可以接受新输入
}

class FDivWrapper extends Module {
    val io = IO(new FDivWrapperIO)
    
    // 实例化fudian的FDIV模块
    val fdiv = Module(new FDIV(expWidth, precision))
    
    // 判断是否是 FSQRT
    val isSqrt = io.op === FSQRT_S
    
    // 连接输入
    fdiv.io.a := io.rs1Data
    fdiv.io.b := io.rs2Data
    fdiv.io.rm := io.rm
    
    // 握手信号
    fdiv.io.specialIO.isSqrt := isSqrt
    fdiv.io.specialIO.in_valid := io.valid
    fdiv.io.specialIO.out_ready := true.B  // 总是准备好接收输出
    fdiv.io.specialIO.kill := io.kill
    
    // 输出信号
    io.res := fdiv.io.result
    io.fflags := fdiv.io.fflags
    io.busy := !fdiv.io.specialIO.in_ready
    io.ready := fdiv.io.specialIO.in_ready
}

