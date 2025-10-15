import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.Dispatch._
import ZirconUtil._

class DispatcherIO extends Bundle {
    // nfch(4)个前端Package输入
    val ftePkg = Vec(nfch, Flipped(Decoupled(new BackendPackage)))

    // nfch个nfch+2位宽向量，指示指令被发到哪个FU
    val func   = Input(Vec(nfch, UInt((nfuncUnit).W)))

    // nfch个后端Package输出
    val bkePkg = Vec(nfch, Decoupled(new BackendPackage))

}

class Dispatcher extends Module {
    val io = IO(new DispatcherIO)

    // 所有后端Package ready
    val bkePkgAllReady = io.bkePkg.map(_.ready).reduce(_ && _)

    // 初始化输出端口，默认无效
    io.bkePkg.foreach { pkg =>
        pkg.valid := false.B
        pkg.bits  := DontCare
    }

    val funcMaskTable = Wire(Vec(nfch + 1, Vec(nfch, UInt(nfuncUnit.W))))
    val funcIdTable = Wire(Vec(nfch, UInt(nfuncUnit.W)))
    val ftePkgTable = Wire(Vec(nfch, (new BackendPackage)))


    funcMaskTable(0) := io.func

    // Generate nfch stages of priority logic
    for (stage <- 0 until nfch) {
        // 寻找可用FU最少的指令
        // 统计当前所有指令可用FU数
        // TODO
        val currentPopCntList = funcMaskTable(stage).map(PopCount(_))
        
        // 选出可用FU最少的指令和索引
        val (min_pop, min_idx) = currentPopCntList.zipWithIndex
            .map { case (p, i) => (Mux(p === 0.U, (nfuncUnit + 1).U, p), (1<<i).U) } // 忽略已无请求的指令
            // reduce-reduceTree
            .reduce[(UInt, UInt)] { case ((p1, i1), (p2, i2)) =>
            val choose_p1 = p1 < p2
            val min_p_result = Mux(choose_p1, p1, p2)
            val min_i_result = Mux(choose_p1, i1, i2)
            (min_p_result, min_i_result)
        }

        // 分配FU
        // log2OH PriorityMux
        // 对可用FU最少的一条指令掩码进行优先级编码
        val fuAllocOh = PriorityEncoderOH(Mux1H(min_idx,funcMaskTable(stage)))
        funcIdTable(stage) := fuAllocOh
        ftePkgTable(stage) := Mux1H(min_idx,io.ftePkg.map(_.bits)) 
        // io.bkePkg(fuIdx).valid := true.B
        // io.bkePkg(fuIdx).bits  := io.ftePkg(min_idx).bits

        // 可用FU矩阵更新
        for (instr_idx <- 0 until nfch) {
            val Mask = ~fuAllocOh // 掩码，清除已被占用的FU
            val current_request = funcMaskTable(stage)(instr_idx)
            funcMaskTable(stage + 1)(instr_idx) := Mux(min_idx(instr_idx), 0.U,current_request & Mask) 
        }
    }
    val funcIdTableTrans = Transpose(funcIdTable)
    io.bkePkg.zipWithIndex.foreach{case (p , i) => 
        p.valid := true.B
        p.bits := Mux1H(funcIdTableTrans(i),ftePkgTable)
    }

    // --- 反压信号 ---
    io.ftePkg.foreach { ftePkg => ftePkg.ready := bkePkgAllReady }
}
