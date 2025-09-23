import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._

class ForwardIO extends Bundle {
    val instPkgEX1 = Input(Vec(nfch, new BackendPackage))
    val instPkgEX2 = Input(Vec(nfch, new BackendPackage))
    val instPkgEX3 = Input(Vec(nfch, new BackendPackage))
    val instPkgWB = Input(Vec(nfch, new BackendPackage))
    val src1Fwd = Vec(nfch, Decoupled(UInt(32.W)))
    val src2Fwd = Vec(nfch, Decoupled(UInt(32.W)))
}

class Forward extends Module {
    val io = IO(new ForwardIO)
    io.src1Fwd.zipWithIndex.foreach{ case (src1Fwd, i) =>
        val fwdEnEX2 = VecInit.tabulate(nfch)(j => (
            io.instPkgEX1(i).rj === io.instPkgEX2(i).rd && io.instPkgEX2(i).rdVld
        )).asUInt
        val fwdEnEX3 = VecInit.tabulate(nfch)(j => (
            io.instPkgEX1(i).rj === io.instPkgEX3(j).rd && io.instPkgEX3(j).rdVld
        )).asUInt
        val fwdEnWB = VecInit.tabulate(nfch)(j => (
            io.instPkgEX1(i).rj === io.instPkgWB(j).rd && io.instPkgWB(j).rdVld
        )).asUInt
        val fwdEn = fwdEnEX2.orR || fwdEnEX3.orR || fwdEnWB.orR
        src1Fwd.bits := Mux(fwdEnEX2.orR,
            Mux1H(fwdEnEX2.asBools, io.instPkgEX2.map(_.rfWdata)),
            Mux(fwdEnEX3.orR,
                Mux1H(fwdEnEX3.asBools, io.instPkgEX3.map(_.rfWdata)),
                Mux1H(fwdEnWB.asBools, io.instPkgWB.map(_.rfWdata))
            )
        )
        src1Fwd.valid := fwdEn
    }
    io.src2Fwd.zipWithIndex.foreach{ case (src2Fwd, i) =>
        val fwdEnEX2 = VecInit.tabulate(nfch)(j => (
            io.instPkgEX1(i).rk === io.instPkgEX2(j).rd && io.instPkgEX2(j).rdVld
        )).asUInt
        val fwdEnEX3 = VecInit.tabulate(nfch)(j => (
            io.instPkgEX1(i).rk === io.instPkgEX3(j).rd && io.instPkgEX3(j).rdVld
        )).asUInt
        val fwdEnWB = VecInit.tabulate(nfch)(j => (
            io.instPkgEX1(i).rk === io.instPkgWB(j).rd && io.instPkgWB(j).rdVld
        )).asUInt
        val fwdEn = fwdEnEX2.orR || fwdEnEX3.orR || fwdEnWB.orR
        src2Fwd.valid := fwdEn
        src2Fwd.bits := Mux(fwdEnEX2.orR,
            Mux1H(fwdEnEX2.asBools, io.instPkgEX2.map(_.rfWdata)),
            Mux(fwdEnEX3.orR,
                Mux1H(fwdEnEX3.asBools, io.instPkgEX3.map(_.rfWdata)),
                Mux1H(fwdEnWB.asBools, io.instPkgWB.map(_.rfWdata))
            )
        )
    }
}