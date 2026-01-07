import chisel3._
import chisel3.util._
import fudian._

// FPU IO接口 - 适配流水线调用
class FPUIO extends Bundle {
    val rs1Data = Input(UInt(32.W))
    val rs2Data = Input(UInt(32.W))
    val rs3Data = Input(UInt(32.W))
    val op      = Input(UInt(7.W))
    val rm      = Input(UInt(3.W))  // rounding mode
    val res     = Output(UInt(32.W))
    val fflags  = Output(UInt(5.W))
}

// 完整的浮点运算单元
// 支持：FADD, FSUB, FMUL, FSGNJ系列, FCMP系列, FMIN/FMAX, FCLASS, FMV
class FPU extends Module {
    val io = IO(new FPUIO)
    
    // 使用统一的浮点参数配置
    val expWidth = ZirconConfig.FloatConfig.expWidth
    val precision = ZirconConfig.FloatConfig.precision
    
    // 操作码解析
    val op = io.op
    
    // 指令判断（基于 Config.scala 中的 EXEOp 定义）
    val isFADD   = op === ZirconConfig.EXEOp.FADD_S
    val isFSUB   = op === ZirconConfig.EXEOp.FSUB_S
    val isFMUL   = op === ZirconConfig.EXEOp.FMUL_S
    val isFSGNJ  = op === ZirconConfig.EXEOp.FSGNJ_S
    val isFSGNJN = op === ZirconConfig.EXEOp.FSGNJN_S
    val isFSGNJX = op === ZirconConfig.EXEOp.FSGNJX_S
    val isFMIN   = op === ZirconConfig.EXEOp.FMIN_S
    val isFMAX   = op === ZirconConfig.EXEOp.FMAX_S
    val isFLE    = op === ZirconConfig.EXEOp.FLE_S
    val isFLT    = op === ZirconConfig.EXEOp.FLT_S
    val isFEQ    = op === ZirconConfig.EXEOp.FEQ_S
    val isFCLASS = op === ZirconConfig.EXEOp.FCLASS_S
    val isFMV_X_W = op === ZirconConfig.EXEOp.FMV_X_W
    val isFMV_W_X = op === ZirconConfig.EXEOp.FMV_W_X
    
    // 分类信号
    val isAddSub = isFADD || isFSUB
    val isMul = isFMUL
    val isSgnj = isFSGNJ || isFSGNJN || isFSGNJX
    val isMinMax = isFMIN || isFMAX
    val isCmp = isFLE || isFLT || isFEQ
    
    // ========== FADD/FSUB 模块 ==========
    val signBit = ZirconConfig.FloatConfig.signBit
    val fadd = Module(new FCMA_ADD(expWidth, precision, precision))
    val subSign = Mux(isFSUB, ~io.rs2Data(signBit), io.rs2Data(signBit))
    fadd.io.a := io.rs1Data
    fadd.io.b := subSign ## io.rs2Data(signBit - 1, 0)
    fadd.io.b_inter_valid := false.B
    fadd.io.b_inter_flags := DontCare
    fadd.io.rm := io.rm
    
    // ========== FMUL 模块 ==========
    val fmul = Module(new FMUL(expWidth, precision))
    fmul.io.a := io.rs1Data
    fmul.io.b := io.rs2Data
    fmul.io.rm := io.rm
    
    // ========== FCMP 模块 ==========
    val fcmp = Module(new FCMP(expWidth, precision))
    fcmp.io.a := io.rs1Data
    fcmp.io.b := io.rs2Data
    // FEQ 不是 signaling，FLT/FLE 是 signaling
    fcmp.io.signaling := isFLT || isFLE
    
    // ========== 符号注入 (FSGNJ系列) ==========
    // FSGNJ:  result = |rs1| with sign of rs2
    // FSGNJN: result = |rs1| with negated sign of rs2  
    // FSGNJX: result = |rs1| with XOR of signs
    val rs1Sign = io.rs1Data(signBit)
    val rs2Sign = io.rs2Data(signBit)
    val sgnjSign = Mux1H(Seq(
        isFSGNJ  -> rs2Sign,
        isFSGNJN -> (~rs2Sign),
        isFSGNJX -> (rs1Sign ^ rs2Sign)
    ))
    val sgnjResult = sgnjSign ## io.rs1Data(signBit - 1, 0)
    
    // ========== FMIN/FMAX ==========
    // 使用 FCMP 的比较结果
    val minMaxResult = Mux(isFMIN,
        Mux(fcmp.io.lt, io.rs1Data, io.rs2Data),
        Mux(fcmp.io.lt, io.rs2Data, io.rs1Data)
    )
    
    // ========== FCLASS ==========
    // 分类结果是10位独热码
    val fp = FloatPoint.fromUInt(io.rs1Data, expWidth, precision)
    val decode = fp.decode
    val sign = fp.sign
    
    val fclassResult = Cat(
        0.U(22.W),
        decode.isQNaN,                           // bit 9: quiet NaN
        decode.isSNaN,                           // bit 8: signaling NaN
        !sign && decode.isInf,                   // bit 7: +infinity
        !sign && !decode.expIsZero && !decode.expIsOnes,  // bit 6: +normal
        !sign && decode.isSubnormal,             // bit 5: +subnormal
        !sign && decode.isZero,                  // bit 4: +0
        sign && decode.isZero,                   // bit 3: -0
        sign && decode.isSubnormal,              // bit 2: -subnormal
        sign && !decode.expIsZero && !decode.expIsOnes,   // bit 1: -normal
        sign && decode.isInf                     // bit 0: -infinity
    )
    
    // ========== 比较结果 ==========
    val cmpResult = Cat(0.U(31.W), Mux1H(Seq(
        isFEQ -> fcmp.io.eq,
        isFLT -> fcmp.io.lt,
        isFLE -> fcmp.io.le
    )))
    
    // ========== FMV 结果 ==========
    // FMV.X.W: 直接传递位模式（浮点->整数寄存器）
    // FMV.W.X: 直接传递位模式（整数->浮点寄存器）
    val fmvResult = io.rs1Data
    
    // ========== 结果选择（需要考虑流水延迟）==========
    // FADD/FSUB/FMUL 有3级流水延迟，其他是组合逻辑
    // 这里只输出组合逻辑结果，流水延迟在流水线中处理
    
    // 组合逻辑结果（无延迟）
    val combResult = Mux1H(Seq(
        isSgnj    -> sgnjResult,
        isMinMax  -> minMaxResult,
        isCmp     -> cmpResult,
        isFCLASS  -> fclassResult,
        (isFMV_X_W || isFMV_W_X) -> fmvResult
    ))
    
    // 流水结果（有延迟）- FADD/FSUB 2级，FMUL 3级
    // 注意：FCMA_ADD 内部已有1级寄存器，所以 fadd 结果延迟2拍
    // FMUL 内部有2级寄存器，所以 fmul 结果延迟3拍
    val faddResult = fadd.io.result
    val fmulResult = fmul.io.result
    
    // 最终结果选择
    // 对于需要流水的操作，在 EX3 阶段取结果
    // 对于组合逻辑操作，在 EX1 阶段就有结果，但为了统一在 EX3 取
    io.res := Mux(isMul, fmulResult, 
              Mux(isAddSub, faddResult, combResult))
    
    // ========== fflags 选择 ==========
    val addFflags = fadd.io.fflags
    val mulFflags = fmul.io.fflags
    val cmpFflags = fcmp.io.fflags
    
    // 组合逻辑操作的 fflags
    val combFflags = Mux(isCmp || isMinMax, cmpFflags, 0.U(5.W))
    
    io.fflags := Mux(isMul, mulFflags,
                 Mux(isAddSub, addFflags, combFflags))
}
