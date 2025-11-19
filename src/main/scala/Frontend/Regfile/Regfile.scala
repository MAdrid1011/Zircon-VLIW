import chisel3._
import chisel3.util._

class RegfileIO(nr: Int, nw: Int) extends Bundle {
    val raddr = Input(Vec(nr, UInt(5.W)))
    val rdata = Output(Vec(nr, UInt(32.W)))
    val waddr = Input(Vec(nw, UInt(5.W)))
    val wdata = Input(Vec(nw, UInt(32.W)))
    val wen = Input(Vec(nw, Bool()))
}

class Regfile(nr: Int, nw: Int) extends Module {
    val io = IO(new RegfileIO(nr, nw))
    val regfile = RegInit(VecInit.fill(32)(0.U(32.W)))
    
    // 读端口逻辑：每个读端口对所有写端口判断写优先
    for (i <- 0 until nr) {
        val forwardMatch = Wire(Vec(nw, Bool()))
        for (j <- 0 until nw) {
            forwardMatch(j) := io.wen(j) && (io.waddr(j) === io.raddr(i))
        }
        
        // 写优先：如果有写端口匹配，使用写数据；否则使用寄存器堆数据
        io.rdata(i) := MuxCase(regfile(io.raddr(i)), 
            (0 until nw).map(j => (forwardMatch(j), io.wdata(j)))
        )
    }
    
    // 写端口逻辑
    for (i <- 0 until nw) {
        when(io.wen(i)) {
            regfile(io.waddr(i)) := io.wdata(i)
        }
    }
}
