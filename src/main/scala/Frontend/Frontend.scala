import chisel3._ 
import chisel3.util._
import ZirconConfig.Fetch._

class FrontendMemIO extends Bundle {
    val faddr = Output(UInt(32.W))
    val inst = Input(Vec(nfch, UInt(32.W)))
}

class FrontendDispatchIO extends Bundle {
    val instPkg = Vec(nfch, Decoupled(new FrontendPackage))
}

class FrontendIO extends Bundle {
    val dsp = new FrontendDispatchIO
    val mem = new FrontendMemIO
}


class Frontend extends Module {
    val io = IO(new FrontendIO)

    val npc = Module(new NPC)
    val dcd = VecInit.fill(nfch)(Module(new Decoder).io)
    val pc  = RegInit(0x7FFFFFE8.U(32.W))

    /* Previous Fetch Stage */
    val instPkgPF = WireDefault(VecInit.fill(nfch)(0.U.asTypeOf(new FrontendPackage)))
    val instPkgFCIn = WireDefault(instPkgPF)
    // TODO
    npc.io.cmt := DontCare
    npc.io.fch.pc := pc

    instPkgPF.zip(npc.io.fch.validMask).foreach{ case (pkg, mask) =>
        pkg.valid := mask
    }
    /* Fetch Stage */
    val instPkgFC = WireDefault(ShiftRegister(
        instPkgFCIn,
        1,
        0.U.asTypeOf(Vec(nfch, new FrontendPackage)),
        true.B
    ))
    io.mem.faddr := npc.io.fch.npc
    val pcs = VecInit.tabulate(nfch)(i => BLevelPAdder32(pc, (i * 4).U, 0.U).io.res)

    val instPkgDCDIn = WireDefault(instPkgFC)
    instPkgDCDIn.zipWithIndex.foreach{ case (pkg, i) =>
        pkg.pc := pcs(i)
        pkg.inst := io.mem.inst(i)
    }
    val instPkgDCD = WireDefault(ShiftRegister(
        instPkgDCDIn,
        1,
        0.U.asTypeOf(Vec(nfch, new FrontendPackage)),
        true.B
    ))
    val instPkgDSPIn = WireDefault(instPkgDCD)
    dcd.zip(instPkgDCD).foreach{ case (dcd, pkg) =>
        dcd.inst := pkg.inst
    }
    dcd.zip(instPkgDSPIn).foreach{ case (dcd, pkg) =>
        pkg.op := dcd.op
        pkg.imm := dcd.imm
        pkg.func := Mux(pkg.valid, dcd.func, 0.U(nfch.W))
        pkg.rinfo := dcd.rinfo
    }
    val instPkgDSP = WireDefault(ShiftRegister(
        instPkgDSPIn,
        1,
        0.U.asTypeOf(Vec(nfch, new FrontendPackage)),
        true.B
    ))
    io.dsp.instPkg.zip(instPkgDSP).foreach{ case (dsp, pkg) =>
        dsp.bits := pkg
        dsp.valid := pkg.valid
    }

    

    
}