import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import ZirconConfig.FloatConfig._
import fudian._

/**
 * FPU包装类 - 封装fudian浮点运算模块
 * 支持: FADD, FSUB, FMUL, FEQ, FLT, FLE, FSGNJ, FSGNJN, FSGNJX, 
 *       FMIN, FMAX, FCLASS, FMV.X.W, FMV.W.X
 * 不支持: FCMA系列（FMADD, FMSUB, FNMADD, FNMSUB）
 */
class FPUIO extends Bundle {
    val rs1Data = Input(UInt(32.W))    // 源操作数1
    val rs2Data = Input(UInt(32.W))    // 源操作数2
    val rs3Data = Input(UInt(32.W))    // 源操作数3（保留，本次不用）
    val op = Input(UInt(7.W))          // 操作码
    val rm = Input(UInt(3.W))          // 舍入模式
    val res = Output(UInt(32.W))       // 运算结果
    val fflags = Output(UInt(5.W))     // 浮点异常标志 {NV, DZ, OF, UF, NX}
}

class FPU extends Module {
    val io = IO(new FPUIO)
    
    // 实例化fudian模块
    val fadd = Module(new FADD(expWidth, precision))
    val fmul = Module(new FMUL(expWidth, precision))
    val fcmp = Module(new FCMP(expWidth, precision))
    
    // 默认值
    val defaultRes = 0.U(32.W)
    val defaultFflags = 0.U(5.W)
    
    // ========== 算术运算模块连接 ==========
    // FADD/FSUB
    fadd.io.a := io.rs1Data
    // FSUB通过翻转rs2的符号位实现
    val isFSUB = io.op === FSUB_S
    fadd.io.b := Mux(isFSUB, 
        Cat(~io.rs2Data(31), io.rs2Data(30, 0)),  // 翻转符号位
        io.rs2Data
    )
    fadd.io.rm := io.rm
    
    // FMUL
    fmul.io.a := io.rs1Data
    fmul.io.b := io.rs2Data
    fmul.io.rm := io.rm
    
    // FCMP - 比较运算
    fcmp.io.a := io.rs1Data
    fcmp.io.b := io.rs2Data
    fcmp.io.signaling := (io.op === FLE_S || io.op === FLT_S)  // FLE和FLT是信号型比较
    
    // ========== 符号注入逻辑 ==========
    val rs1_sign = io.rs1Data(31)
    val rs2_sign = io.rs2Data(31)
    val rs1_abs = io.rs1Data(30, 0)
    
    val fsgnj_res = Wire(UInt(32.W))
    fsgnj_res := MuxCase(io.rs1Data, Seq(
        (io.op === FSGNJ_S)  -> Cat(rs2_sign, rs1_abs),      // 使用rs2的符号
        (io.op === FSGNJN_S) -> Cat(~rs2_sign, rs1_abs),     // 使用rs2符号的反
        (io.op === FSGNJX_S) -> Cat(rs1_sign ^ rs2_sign, rs1_abs)  // 异或
    ))
    
    // ========== MIN/MAX逻辑 ==========
    val fmin_max_res = Wire(UInt(32.W))
    // 简化实现：使用fcmp的结果
    val a_lt_b = fcmp.io.lt
    val a_le_b = fcmp.io.le
    fmin_max_res := MuxCase(io.rs1Data, Seq(
        (io.op === FMIN_S) -> Mux(a_le_b, io.rs1Data, io.rs2Data),
        (io.op === FMAX_S) -> Mux(a_le_b, io.rs2Data, io.rs1Data)
    ))
    
    // ========== FCLASS逻辑 ==========
    val fp_a = FloatPoint.fromUInt(io.rs1Data, expWidth, precision)
    val decode_a = fp_a.decode
    val fclass_res = Wire(UInt(32.W))
    // FCLASS返回一个10位的分类掩码
    val class_bits = Cat(
        decode_a.isQNaN,                                    // bit 9: quiet NaN
        decode_a.isSNaN,                                    // bit 8: signaling NaN
        !fp_a.sign && decode_a.isInf,                      // bit 7: +infinity
        !fp_a.sign && !decode_a.isZero && !decode_a.isInf && !decode_a.isNaN && !decode_a.isSubnormal, // bit 6: +normal
        !fp_a.sign && decode_a.isSubnormal,                // bit 5: +subnormal
        !fp_a.sign && decode_a.isZero,                     // bit 4: +zero
        fp_a.sign && decode_a.isZero,                      // bit 3: -zero
        fp_a.sign && decode_a.isSubnormal,                 // bit 2: -subnormal
        fp_a.sign && !decode_a.isZero && !decode_a.isInf && !decode_a.isNaN && !decode_a.isSubnormal,  // bit 1: -normal
        fp_a.sign && decode_a.isInf                        // bit 0: -infinity
    )
    fclass_res := Cat(0.U(22.W), class_bits)
    
    // ========== FMV逻辑 ==========
    // FMV.X.W: 将浮点寄存器中的位模式移动到整数寄存器（NaN-box检查）
    // FMV.W.X: 将整数寄存器的低32位移动到浮点寄存器
    val fmv_res = Wire(UInt(32.W))
    fmv_res := MuxCase(defaultRes, Seq(
        (io.op === FMV_X_W) -> io.rs1Data,    // 直接传递浮点位模式到整数
        (io.op === FMV_W_X) -> io.rs1Data     // 直接传递整数到浮点
    ))
    
    // ========== 比较结果 ==========
    val fcmp_res = Wire(UInt(32.W))
    fcmp_res := MuxCase(0.U(32.W), Seq(
        (io.op === FEQ_S) -> fcmp.io.eq,
        (io.op === FLT_S) -> fcmp.io.lt,
        (io.op === FLE_S) -> fcmp.io.le
    ))
    
    // ========== 输出选择 ==========
    io.res := MuxCase(defaultRes, Seq(
        (io.op === FADD_S || io.op === FSUB_S) -> fadd.io.result,
        (io.op === FMUL_S) -> fmul.io.result,
        (io.op === FEQ_S || io.op === FLT_S || io.op === FLE_S) -> fcmp_res,
        (io.op === FSGNJ_S || io.op === FSGNJN_S || io.op === FSGNJX_S) -> fsgnj_res,
        (io.op === FMIN_S || io.op === FMAX_S) -> fmin_max_res,
        (io.op === FCLASS_S) -> fclass_res,
        (io.op === FMV_X_W || io.op === FMV_W_X) -> fmv_res
    ))
    
    io.fflags := MuxCase(defaultFflags, Seq(
        (io.op === FADD_S || io.op === FSUB_S) -> fadd.io.fflags,
        (io.op === FMUL_S) -> fmul.io.fflags,
        (io.op === FEQ_S || io.op === FLT_S || io.op === FLE_S) -> fcmp.io.fflags
    ))
}

