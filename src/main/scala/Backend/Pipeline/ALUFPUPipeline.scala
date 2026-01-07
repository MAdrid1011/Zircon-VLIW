import chisel3._
import chisel3.util._

class ALUFPUPipelineIO extends Bundle {
    val forward = new PipelineForwardIO
    val backend = new PipelineBackendIO
    val frontend = new PipelineFrontendIO
    val hazard = new PipelineHazardIO
}

// ALUFPUPipeline 支持类型转换
// convertType: 1 = FPToInt (1号流水线), 0 = IntToFP (2号流水线)
class ALUFPUPipeline(val convertType: Int = 0) extends Module {
    val io = IO(new ALUFPUPipelineIO)
    
    // ========== EX1阶段 ==========
    // ID-EX1 段间寄存器在 Backend 中统一管理，这里直接使用传入的数据
    val ex1Pkg = io.backend.instPkgIn
    
    // 应用Forward前递
    val ex1Rs1Data = io.forward.fwdRs1Data
    val ex1Rs2Data = io.forward.fwdRs2Data
    val ex1Rs3Data = io.forward.fwdRs3Data
    
    // ALU实例化
    val alu = Module(new ALU)
    val aluSrc1 = Mux(ex1Pkg.src1Sel === 0.U, ex1Rs1Data, ex1Pkg.pc)
    val aluSrc2 = Mux(ex1Pkg.src2Sel === 0.U, ex1Rs2Data, ex1Pkg.imm)
    alu.io.src1 := aluSrc1
    alu.io.src2 := aluSrc2
    alu.io.op := ex1Pkg.op
    
    // FPU实例化
    val fpu = Module(new FPU)
    fpu.io.rs1Data := ex1Rs1Data
    fpu.io.rs2Data := ex1Rs2Data
    fpu.io.rs3Data := ex1Rs3Data
    fpu.io.op := ex1Pkg.op
    fpu.io.rm := ex1Pkg.rm
    
    // 类型转换模块实例化
    val fpuConvert = Module(new FPUConvert(convertType))
    fpuConvert.io.rs1Data := ex1Rs1Data
    fpuConvert.io.op := ex1Pkg.op
    fpuConvert.io.rm := ex1Pkg.rm
    
    // EX1阶段更新InstPkg
    val ex1PkgOut = ex1Pkg.EX1Update(alu.io.res, 0.U, false.B)
    
    // ========== EX2阶段 ==========
    val ex2Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex2Flush) {
        ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex2Stall) {
        ex2Pkg := ex1PkgOut
    }
    
    // ========== EX3阶段 ==========
    val ex3Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex3Flush) {
        ex3Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex3Stall) {
        // EX3阶段：选择 FPU 或类型转换结果
        // 保存 EX2 阶段的 isConvert 判断结果
        val ex2IsConvert = if (convertType == 1) {
            ex2Pkg.op === ZirconConfig.EXEOp.FCVT_W_S || 
            ex2Pkg.op === ZirconConfig.EXEOp.FCVT_WU_S
        } else {
            ex2Pkg.op === ZirconConfig.EXEOp.FCVT_S_W || 
            ex2Pkg.op === ZirconConfig.EXEOp.FCVT_S_WU
        }
        
        val fpuRes = Mux(ex2IsConvert, fpuConvert.io.res, fpu.io.res)
        val fpuFlags = Mux(ex2IsConvert, fpuConvert.io.fflags, fpu.io.fflags)
        ex3Pkg := ex2Pkg.EX3Update(fpuRes, fpuFlags)
    }
    
    // ========== WB阶段 ==========
    val wbPkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.wbFlush) {
        wbPkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.wbStall) {
        wbPkg := ex3Pkg
    }
    
    // WB阶段：选择写回数据（FPU指令用fpuResult，ALU指令用aluResult）
    val isFPU = wbPkg.op(6)  // op[6]表示是否是FPU指令
    val wbData = Mux(isFPU, wbPkg.fpuResult, wbPkg.aluResult)
    val wbPkgOut = wbPkg.WBUpdate(wbData)
    
    // 写回到寄存器堆
    io.frontend.gprWen := wbPkgOut.rdValid && !wbPkgOut.rd(5)
    io.frontend.gprWaddr := wbPkgOut.rd(4, 0)
    io.frontend.gprWdata := wbPkgOut.rfWdata
    io.frontend.fprWen := wbPkgOut.rdValid && wbPkgOut.rd(5)
    io.frontend.fprWaddr := wbPkgOut.rd(4, 0)
    io.frontend.fprWdata := wbPkgOut.rfWdata
    
    // 输出到Forward和Hazard
    io.forward.ex1Pkg := ex1Pkg
    io.forward.ex2Pkg := ex2Pkg
    io.forward.ex3Pkg := ex3Pkg
    io.forward.wbPkg := wbPkgOut
    io.hazard.ex1Pkg := ex1Pkg
    io.hazard.ex2Pkg := ex2Pkg
}
