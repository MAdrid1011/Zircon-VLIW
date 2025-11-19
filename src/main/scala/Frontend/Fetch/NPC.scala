import chisel3._
import chisel3.util._
class NPCBackendIO extends Bundle {
    val branchTgt   = Input(UInt(32.W))
    val predFail    = Input(Bool())
    val stall       = Input(Bool())
}
class NPCFetchIO extends Bundle {
    val pc         = Input(UInt(32.W))
    val npc        = Output(UInt(32.W))
}
class NPCIO extends Bundle {
    val backend = new NPCBackendIO
    val fetch = new NPCFetchIO
}
class NPC extends Module {
    val io = IO(new NPCIO)
    val npc = WireDefault(io.fetch.pc)
    when(io.backend.predFail){
        npc := io.backend.branchTgt
    }.elsewhen(io.backend.stall){
        npc := io.fetch.pc
    }.otherwise{
        npc := io.fetch.pc + 32.U  // 8条指令，每条4字节，所以是+32
    }
    io.fetch.npc := npc
}