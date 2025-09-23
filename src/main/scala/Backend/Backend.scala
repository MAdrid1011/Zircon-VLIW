import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._


class BackendMemIO extends Bundle {
    val maddr = Output(UInt(32.W))
    val wstrb = Output(UInt(4.W))
    val wdata = Output(UInt(32.W))
    val rdata = Input(UInt(32.W))
}

class BackendDispatchIO extends Bundle {
    val instPkg = Vec(nfch, Flipped(Decoupled(new BackendPackage)))
}

class BackendIO extends Bundle {
    val mem = new BackendMemIO
    val dsp = new BackendDispatchIO
}

class Backend extends Module {
    val io = IO(new BackendIO)

    val rf = Module(new Regfile)
    val alu = VecInit.fill(4)(Module(new ALU).io)
    val branch = Module(new Branch)
    val iMul = Module(new MulBooth2Wallce)
    val iDiv = Module(new SRT2)
    val fwd = Module(new Forward)

    val instPkgRF = ShiftRegister(
        VecInit(io.dsp.instPkg.map(_.bits)),
        1,
        0.U.asTypeOf(Vec(nfch, new BackendPackage)),
        true.B
    )
    val instPkgEX1In = WireDefault(instPkgRF)
    instPkgEX1In.zipWithIndex.foreach{ case (pkg, i) =>
        rf.io.rd.rj(i) := pkg.rj
        rf.io.rd.rk(i) := pkg.rk
        pkg.src1 := rf.io.rd.rjData(i)
        pkg.src2 := rf.io.rd.rkData(i)
    }
    val instPkgEX1 = WireDefault(ShiftRegister(
        instPkgEX1In,
        1,
        0.U.asTypeOf(Vec(nfch, new BackendPackage)),
        true.B
    ))
    fwd.io.instPkgEX1 := instPkgEX1
    val instPkgEX2In = WireDefault(instPkgEX1)
    // alu
    alu.zipWithIndex.foreach{ case (alu, i) =>
        alu.src1 := Mux(instPkgEX1(i).op(8), instPkgEX1(i).pc, Mux(fwd.io.src1Fwd(i).valid, fwd.io.src1Fwd(i).bits, instPkgEX1(i).src1))
        alu.src2 := Mux(instPkgEX1(i).op(7), instPkgEX1(i).src2, Mux(fwd.io.src2Fwd(i).valid, fwd.io.src2Fwd(i).bits, instPkgEX1(i).imm))
        alu.op := (if(i == 0) Mux(instPkgEX1(i).op(5, 4).orR, 0.U(5.W), instPkgEX1(i).op(4, 0)) else instPkgEX1(i).op(4, 0))
        instPkgEX2In(i).rfWdata := alu.res
    }
    
    // branch
    branch.io.src1       := Mux(fwd.io.src1Fwd(1).valid, fwd.io.src1Fwd(1).bits, instPkgEX1(1).src1)
    branch.io.src2       := Mux(fwd.io.src2Fwd(1).valid, fwd.io.src2Fwd(1).bits, instPkgEX1(1).src2)
    branch.io.op         := instPkgEX1(1).op
    branch.io.pc         := instPkgEX1(1).pc
    branch.io.imm        := instPkgEX1(1).imm
    branch.io.predOffset := instPkgEX1(1).predOffset
    instPkgEX2In(1).jumpEn      := branch.io.realJp
    instPkgEX2In(1).predFail    := branch.io.predFail
    instPkgEX2In(1).result      := branch.io.jumpTgt

    // mul
    iMul.io.src1 := Mux(fwd.io.src1Fwd(3).valid, fwd.io.src1Fwd(3).bits, instPkgEX1(3).src1)
    iMul.io.src2 := Mux(fwd.io.src2Fwd(3).valid, fwd.io.src2Fwd(3).bits, instPkgEX1(3).src2)
    iMul.io.op := instPkgEX1(3).op

    // div
    iDiv.io.src1 := Mux(fwd.io.src1Fwd(3).valid, fwd.io.src1Fwd(3).bits, instPkgEX1(3).src1)
    iDiv.io.src2 := Mux(fwd.io.src2Fwd(3).valid, fwd.io.src2Fwd(3).bits, instPkgEX1(3).src2)
    iDiv.io.op := instPkgEX1(3).op

    instPkgEX2In.zipWithIndex.foreach{ case (pkg, i) =>
        pkg.src2 := Mux(fwd.io.src2Fwd(i).valid, fwd.io.src2Fwd(i).bits, instPkgEX1(i).src2)
    }

    val instPkgEX2 = ShiftRegister(
        instPkgEX2In,
        1,
        0.U.asTypeOf(Vec(nfch, new BackendPackage)),
        true.B
    )
    fwd.io.instPkgEX2 := instPkgEX2
    val instPkgEX3In = WireDefault(instPkgEX2)
    val instPkgEX3 = ShiftRegister(
        instPkgEX3In,
        1,
        0.U.asTypeOf(Vec(nfch, new BackendPackage)),
        true.B
    )
    fwd.io.instPkgEX3 := instPkgEX3
    val instPkgWBIn = WireDefault(instPkgEX3)
    // mul and div
    iMul.io.divBusy := iDiv.io.busy
    instPkgWBIn(3).rfWdata := Mux(instPkgEX3(3).op(4), Mux(instPkgEX3(3).op(2), iDiv.io.res, iMul.io.res), instPkgEX3(3).rfWdata)
    // load and store
    io.mem.maddr := instPkgEX3(0).rfWdata
    io.mem.wstrb := (MuxLookup(instPkgEX3(0).op(1, 0), 0.U(4.W))(Seq(
        0.U -> 1.U(4.W),
        1.U -> 3.U(4.W),
        2.U -> 0xf.U(4.W),
    )) << instPkgEX3(0).rfWdata(1, 0)) & Fill(4, instPkgEX3(0).op(6))
    io.mem.wdata := instPkgEX3(0).src2
    val rdata = MuxLookup(instPkgEX3(0).op(2, 0), 0.U(32.W))(Seq(
        0.U -> Fill(24, io.mem.rdata(7)) ## io.mem.rdata(7, 0),
        1.U -> Fill(16, io.mem.rdata(15)) ## io.mem.rdata(15, 0),
        2.U -> io.mem.rdata,
        4.U -> 0.U(24.W) ## io.mem.rdata(7, 0),
        5.U -> 0.U(16.W) ## io.mem.rdata(15, 0),
    ))
    instPkgWBIn(0).rfWdata := Mux(instPkgEX3(0).op(5), rdata, instPkgEX3(0).rfWdata)
    val instPkgWB = ShiftRegister(
        instPkgWBIn,
        1,
        0.U.asTypeOf(Vec(nfch, new BackendPackage)),
        true.B
    )
    instPkgWB.zipWithIndex.foreach{ case (pkg, i) =>
        rf.io.wr.rd(i) := pkg.rd
        rf.io.wr.rdVld(i) := pkg.rdVld
        rf.io.wr.rdData(i) := pkg.rfWdata
    }
    fwd.io.instPkgWB := instPkgWB
    fwd.io.src1Fwd.zipWithIndex.foreach{ case (src1Fwd, i) =>
        src1Fwd.ready := true.B
    }
    fwd.io.src2Fwd.zipWithIndex.foreach{ case (src2Fwd, i) =>
        src2Fwd.ready := true.B
    }
    io.dsp.instPkg.foreach{ pkg =>
        pkg.ready := true.B
    }
}