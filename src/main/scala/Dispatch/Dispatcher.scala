import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.Dispatch._
import ZirconUtil._

class DispatcherIO extends Bundle {
    // nfch(4)个前端Package输入
    val ftePkg = Vec(nfch, Flipped(Decoupled(new BackendPackage)))

    // nfch个nfch+2位宽向量，指示指令被发到哪个FU
    val func   = Input(Vec(nfch, UInt((nfunUnit).W)))

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

    val request_matrix = Wire(Vec(nfch + 1, Vec(nfch, UInt(nfunUnit.W))))
    request_matrix(0) := io.func

    // Generate nfch stages of priority logic
    for (stage <- 0 until nfch) {
        // 寻找可用FU最少的指令
        val popcounts = request_matrix(stage).map(PopCount(_))

        val (min_pop, min_idx) = popcounts.zipWithIndex
            .map { case (p, i) => (Mux(p === 0.U, (nfunUnit + 1).U, p), i.U) } // 忽略已无请求的指令
            // reduce-reduceTree
            .reduce[(UInt, UInt)] { case ((p1, i1), (p2, i2)) =>
            val choose_p1 = p1 < p2
            val min_p_result = Mux(choose_p1, p1, p2)
            val min_i_result = Mux(choose_p1, i1, i2)
            (min_p_result, min_i_result)
        }

        // 分配FU
        // log2OH PriorityMux
        val chosen_fu_oh = PriorityEncoderOH(request_matrix(stage)(min_idx))
        // 可用FU矩阵更新
        for (instr_idx <- 0 until nfch) {
            // 如果当前指令在本阶段被授权，下一阶段它的所有请求都清零
            // 同时，所有其他指令的请求中，已经被占用的FU位也需要被清零
            val Mask = ~chosen_fu_oh(stage) // 掩码，清除已被占用的FU
            request_matrix(stage + 1)(instr_idx) := request_matrix(stage)(instr_idx) & Mask
        }
    }

    // 驱动bkePkg
    for (fu_idx <- 0 until nfunUnit) {
        val grant_vec_for_fu = request_matrix(nfch)(fu_idx)
        when(grant_vec_for_fu.orR) {
            io.bkePkg(fu_idx).valid := true.B
            io.bkePkg(fu_idx).bits  := Mux1H(grant_vec_for_fu, io.ftePkg.map(_.bits))
        }
    }

    // --- 反压信号 ---
    io.ftePkg.foreach { ftePkg => ftePkg.ready := bkePkgAllReady }
}
