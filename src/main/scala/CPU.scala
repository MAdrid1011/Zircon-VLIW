import chisel3._ 
import chisel3.util._



class CPUIO extends Bundle {
    val fte =  new FrontendMemIO
    val bke =  new BackendMemIO
}

class CPU extends Module {
    val io = IO(new CPUIO)

    val fte = Module(new Frontend)
    val dsp = Module(new Dispatch)
    val bke = Module(new Backend)

    fte.io.mem <> io.fte
    bke.io.mem <> io.bke
    fte.io.dsp <> dsp.io.fte
    bke.io.dsp <> dsp.io.bke

}