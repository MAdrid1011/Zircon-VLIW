import chisel3._
import chisel3.util._

// 前端内存接口（用于取指）
class FrontendMemIO extends Bundle {
    val pc = Output(UInt(32.W))  // 发送PC到仿真环境
    val insts = Input(Vec(8, UInt(32.W)))  // 接收8条连续的指令
}

// 前端与后端接口
class FrontendBackendIO extends Bundle {
    val instPkg = Output(Vec(8, new InstructionPackage))  // 8个指令包输出
    val branchTgt = Input(UInt(32.W))  // 分支重定向目标地址
    val predFail = Input(Bool())  // 分支预测失败信号
    // 写回接口
    val gprWen = Input(Vec(7, Bool()))
    val gprWaddr = Input(Vec(7, UInt(5.W)))
    val gprWdata = Input(Vec(7, UInt(32.W)))
    val fprWen = Input(Vec(3, Bool()))
    val fprWaddr = Input(Vec(3, UInt(5.W)))
    val fprWdata = Input(Vec(3, UInt(32.W)))
}

// 前端与Hazard接口
class FrontendHazardIO extends Bundle {
    val flush = Input(Bool())  // 冲刷信号
    val stall = Input(Bool())  // 阻塞信号
}

class FrontendIO extends Bundle {
    val mem = new FrontendMemIO
    val backend = new FrontendBackendIO
    val hazard = new FrontendHazardIO
}

class Frontend extends Module {
    val io = IO(new FrontendIO)
    
    // ========== IF Stage ==========
    // PC寄存器
    val pc = RegInit(0.U(32.W))
    
    // NPC模块
    val npc = Module(new NPC)
    npc.io.fetch.pc := pc
    npc.io.backend.branchTgt := io.backend.branchTgt
    npc.io.backend.predFail := io.backend.predFail
    npc.io.backend.stall := io.hazard.stall
    
    // 更新PC
    when(!io.hazard.stall) {
        pc := npc.io.fetch.npc
    }
    
    // 向仿真环境发送PC取指
    io.mem.pc := pc
    
    // IF段的8个InstructionPackage
    val ifInstPkgs = Wire(Vec(8, new InstructionPackage))
    for (i <- 0 until 8) {
        ifInstPkgs(i) := (new InstructionPackage).IFUpdate(pc + (i * 4).U, io.mem.insts(i))
    }
    
    // ========== IF-ID 段间寄存器 ==========
    val idInstPkgs = RegInit(VecInit(Seq.fill(8)(0.U.asTypeOf(new InstructionPackage))))
    when(io.hazard.flush) {
        for (i <- 0 until 8) {
            idInstPkgs(i) := 0.U.asTypeOf(new InstructionPackage)
        }
    }.elsewhen(!io.hazard.stall) {
        idInstPkgs := ifInstPkgs
    }
    
    // ========== ID Stage ==========
    // 8个Decoder，根据流水线功能配置
    val decoders = Seq(
        Module(new Decoder(ALU = false, FPU = true,  Branch = false, Mem = false, IMulDiv = false, FDiv = true)),  // 0: FDiv + FPU
        Module(new Decoder(ALU = true,  FPU = true,  Branch = false, Mem = false, IMulDiv = false, FDiv = false)), // 1: ALU + FPU (FPToInt)
        Module(new Decoder(ALU = true,  FPU = true,  Branch = false, Mem = false, IMulDiv = false, FDiv = false)), // 2: ALU + FPU (IntToFP)
        Module(new Decoder(ALU = true,  FPU = false, Branch = false, Mem = false, IMulDiv = true,  FDiv = false)), // 3: ALU + iMulDiv
        Module(new Decoder(ALU = true,  FPU = false, Branch = false, Mem = false, IMulDiv = true,  FDiv = false)), // 4: ALU + iMulDiv
        Module(new Decoder(ALU = true,  FPU = false, Branch = false, Mem = true,  IMulDiv = false, FDiv = false)), // 5: ALU + LSU
        Module(new Decoder(ALU = true,  FPU = false, Branch = false, Mem = true,  IMulDiv = false, FDiv = false)), // 6: ALU + LSU
        Module(new Decoder(ALU = true,  FPU = false, Branch = true,  Mem = false, IMulDiv = false, FDiv = false))  // 7: ALU + Branch
    )
    
    // 连接Decoder输入
    for (i <- 0 until 8) {
        decoders(i).io.instPkgIn := idInstPkgs(i)
    }
    
    // 寄存器堆：GPR 14读7写，FPR 9读3写
    val grf = Module(new Regfile(nr = 14, nw = 7))
    val frf = Module(new Regfile(nr = 9, nw = 3))
    
    // ========== 寄存器堆读端口连接 ==========
    // 根据文档表格，连接读端口
    // GPR读端口分配: 流水线0需要0个，流水线1-7各需要2个
    // FPR读端口分配: 流水线0-2各需要3个，流水线3-7需要0个
    
    // 流水线0: 0个GPR，3个FPR (rs1, rs2, rs3)
    frf.io.raddr(0) := idInstPkgs(0).inst(19, 15)
    frf.io.raddr(1) := idInstPkgs(0).inst(24, 20)
    frf.io.raddr(2) := idInstPkgs(0).inst(31, 27)
    
    // 流水线1-7: 各2个GPR (rs1, rs2)
    for (i <- 1 until 8) {
        grf.io.raddr((i-1)*2)     := idInstPkgs(i).inst(19, 15)
        grf.io.raddr((i-1)*2 + 1) := idInstPkgs(i).inst(24, 20)
    }
    
    // 流水线1-2: 各3个FPR (rs1, rs2, rs3)
    for (i <- 1 until 3) {
        frf.io.raddr(3 + (i-1)*3)     := idInstPkgs(i).inst(19, 15)
        frf.io.raddr(3 + (i-1)*3 + 1) := idInstPkgs(i).inst(24, 20)
        frf.io.raddr(3 + (i-1)*3 + 2) := idInstPkgs(i).inst(31, 27)
    }
    
    // ========== 寄存器堆写端口连接（来自后端）==========
    for (i <- 0 until 7) {
        grf.io.wen(i) := io.backend.gprWen(i)
        grf.io.waddr(i) := io.backend.gprWaddr(i)
        grf.io.wdata(i) := io.backend.gprWdata(i)
    }
    
    for (i <- 0 until 3) {
        frf.io.wen(i) := io.backend.fprWen(i)
        frf.io.waddr(i) := io.backend.fprWaddr(i)
        frf.io.wdata(i) := io.backend.fprWdata(i)
    }
    
    // ========== 组装寄存器数据到InstPkg ==========
    val decodedInstPkgs = Wire(Vec(8, new InstructionPackage))
    
    // 流水线0: 3个FPR数据
    decodedInstPkgs(0) := decoders(0).io.instPkgOut.IDUpdate(
        frf.io.rdata(0),
        frf.io.rdata(1),
        frf.io.rdata(2)
    )
    
    // 流水线1-2: ALU+FPU，根据rs1/rs2/rs3最高位选择GPR或FPR
    for (i <- 1 until 3) {
        val gprBase = (i-1) * 2
        val fprBase = 3 + (i-1) * 3
        val rs1Data = Mux(decoders(i).io.instPkgOut.rs1(5), frf.io.rdata(fprBase),     grf.io.rdata(gprBase))
        val rs2Data = Mux(decoders(i).io.instPkgOut.rs2(5), frf.io.rdata(fprBase + 1), grf.io.rdata(gprBase + 1))
        val rs3Data = Mux(decoders(i).io.instPkgOut.rs3(5), frf.io.rdata(fprBase + 2), 0.U)
        decodedInstPkgs(i) := decoders(i).io.instPkgOut.IDUpdate(rs1Data, rs2Data, rs3Data)
    }
    
    // 流水线3-7: 只有GPR
    for (i <- 3 until 8) {
        val gprBase = (i-1) * 2
        decodedInstPkgs(i) := decoders(i).io.instPkgOut.IDUpdate(
            grf.io.rdata(gprBase),
            grf.io.rdata(gprBase + 1),
            0.U
        )
    }
    
    // 输出到后端
    io.backend.instPkg := decodedInstPkgs
}
