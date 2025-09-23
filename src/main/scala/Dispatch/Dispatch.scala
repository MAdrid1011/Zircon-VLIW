
import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._

class DispatchIO extends Bundle {
    val fte = Flipped(new FrontendDispatchIO)
    val bke = Flipped(new BackendDispatchIO)
}

class Dispatch extends Module {
    val io = IO(new DispatchIO)
    val dsp = Module(new Dispatcher)

    val ftePkgIn = VecInit.tabulate(nfch)(i => (new BackendPackage)(io.fte.instPkg(i).bits))
    val func = io.fte.instPkg.map(_.bits.func)

    dsp.io.ftePkg.zipWithIndex.foreach{ case (ftePkg, i) =>
        ftePkg.valid := io.fte.instPkg(i).valid
        ftePkg.bits := ftePkgIn(i)
        io.fte.instPkg(i).ready := ftePkg.ready
    }
    dsp.io.func := func
    io.bke.instPkg.zipWithIndex.foreach{ case (bkePkg, i) =>
        bkePkg.valid := dsp.io.bkePkg(i).valid
        bkePkg.bits := dsp.io.bkePkg(i).bits
        dsp.io.bkePkg(i).ready := bkePkg.ready
    }

}