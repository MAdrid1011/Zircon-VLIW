import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Fetch._
import ZirconUtil._

class RegfileRDIO extends Bundle{
    val rj     = Input(Vec(nfch, UInt(wlreg.W)))
    val rk     = Input(Vec(nfch, UInt(wlreg.W)))
    val rjData = Output(Vec(nfch, UInt(32.W)))
    val rkData = Output(Vec(nfch, UInt(32.W)))
}
class RegfileWRIO extends Bundle{
    val rd     = Input(Vec(nfch, UInt(wlreg.W)))
    val rdVld  = Input(Vec(nfch, Bool()))
    val rdData = Input(Vec(nfch, UInt(32.W)))
}

class RegfileDBGIO extends Bundle{
    val rf = Output(Vec(nlreg, UInt(32.W)))
}

class RegfileSingleIO extends Bundle{
    val rd = new RegfileRDIO
    val wr = new RegfileWRIO
}

class Regfile extends Module {
    val io  = IO(new RegfileSingleIO)
    val dbg = IO(new RegfileDBGIO)

    val regfile = RegInit(VecInit.tabulate(nlreg)(i => 0.U(32.W)))

    for(i <- 0 until nfch){
        io.rd.rjData(i) := WFirstRead(regfile(io.rd.rj(i)), io.rd.rj(i), io.wr.rd, io.wr.rdData, io.wr.rdVld)
        io.rd.rkData(i) := WFirstRead(regfile(io.rd.rk(i)), io.rd.rk(i), io.wr.rd, io.wr.rdData, io.wr.rdVld)
        when(io.wr.rdVld(i)){
            regfile(io.wr.rd(i)) := io.wr.rdData(i)
        }
    }
    dbg.rf := regfile
}