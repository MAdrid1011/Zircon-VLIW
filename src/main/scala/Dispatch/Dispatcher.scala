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

    funcMaskTable(0) := io.func

    // Generate nfch stages of priority logic
    for (stage <- 0 until nfch) {
        // 寻找可用FU最少的指令
        // 统计当前所有指令可用FU数
        val currentPopCntList = funcMaskTable(stage).map(PopCount(_))
        
        // 选出可用FU最少的指令和索引
        val minIdx = currentPopCntList.zipWithIndex
        .map { case (p, i) => (Mux(p === 0.U, (nfuncUnit + 1).U, p), i.U) } // 忽略已无请求的指令
        // reduce-reduceTree
        .reduce[(UInt, UInt)] { case ((p1, i1), (p2, i2)) =>
            val choose_p1 = p1 < p2
            val min_p_result = Mux(choose_p1, p1, p2)
            val min_i_result = Mux(choose_p1, i1, i2)
            (min_p_result, min_i_result)  // 只保留最小 popcount 和相应索引
        }._2  // 只返回 min_idx (即索引)

        // 分配FU
        // log2OH PriorityMux
        // 对可用FU最少的一条指令掩码进行优先级编码
        val fuAllocOh = PriorityEncoderOH(funcMaskTable(stage)(minIdx))
        val fuIdx = OHToUInt(fuAllocOh)

        io.bkePkg(fuIdx).valid := true.B
        io.bkePkg(fuIdx).bits  := io.ftePkg(minIdx).bits

        // 可用FU矩阵更新
        for (instr_idx <- 0 until nfch) {
            val Mask = ~fuAllocOh // 掩码，清除已被占用的FU
            val current_request = funcMaskTable(stage)(instr_idx)
            funcMaskTable(stage + 1)(instr_idx) := Mux(instr_idx.U === minIdx, 0.U,current_request & Mask) 
        }
    }
    // --- 反压信号 ---
    io.ftePkg.foreach { ftePkg => ftePkg.ready := bkePkgAllReady }
}
