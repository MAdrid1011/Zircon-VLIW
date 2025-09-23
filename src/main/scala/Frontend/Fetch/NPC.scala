import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.JumpOp._

class NPCCommitIO extends Bundle {
    val flush      = Input(Bool())
    val jumpEn     = Input(Bool())
    val jumpTgt    = Input(UInt(32.W))
}
class NPCFetchIO extends Bundle {
    val pc         = Input(UInt(32.W))
    val npc        = Output(UInt(32.W))
    val validMask  = Output(Vec(nfch, Bool()))
}
class NPCIO extends Bundle {
    val cmt = new NPCCommitIO
    val fch  = new NPCFetchIO
}
class NPC extends Module {
    val io             = IO(new NPCIO)
    val pc             = WireDefault(io.fch.pc)
    val offset         = WireDefault((nfch * 4).U)
    when(io.cmt.flush){
        io.fch.npc := BLevelPAdder32(io.cmt.jumpTgt, Mux(io.cmt.jumpEn, 0.U, 4.U), 0.U).io.res
    }.otherwise{
        io.fch.npc := BLevelPAdder32(io.fch.pc, (4 * nfch).U, 0.U).io.res
    }
    io.fch.validMask := VecInit.fill(nfch)(true.B)
}