import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconUtil._

class DispatcherIO extends Bundle {
    val ftePkg = Vec(nfch, Flipped(Decoupled(new BackendPackage)))
    val func   = Input(Vec(nfch, UInt((nfch+2).W)))
    val bkePkg = Vec(nfch, Decoupled(new BackendPackage))
}

class Dispatcher extends Module {
    val io = IO(new DispatcherIO)

    val bkePkgAllReday = io.bkePkg.map(_.ready).reduce(_ && _)
    val aluInstructionIndexes = WireDefault(VecInit.fill(nfch)(0.U(nfch.W)))
    var aluInstructionIndex = 1.U(nfch.W)
    for(i <- 0 until nfch){
        aluInstructionIndexes(i) := Mux(io.func(i)(nfch+1), aluInstructionIndex, 0.U(nfch.W))
        aluInstructionIndex = Mux(io.func(i)(nfch+1), ShiftAdd1(aluInstructionIndex), aluInstructionIndex)
    }
    val fpuInstructionIndexes = WireDefault(VecInit.fill(nfch)(0.U(nfch.W)))
    var fpuInstructionIndex = 1.U(nfch.W)
    for(i <- 0 until nfch){
        fpuInstructionIndexes(i) := Mux(io.func(i)(nfch), fpuInstructionIndex, 0.U(nfch.W))
        fpuInstructionIndex = Mux(io.func(i)(nfch), ShiftAdd1(fpuInstructionIndex), fpuInstructionIndex)
    }
    val intFuncMap = WireDefault(VecInit.fill(4)(0.U(nfch.W)))
    val intFuncHit = WireDefault(VecInit.fill(4)(false.B))
    val floatFuncMap = WireDefault(VecInit.fill(2)(0.U(nfch.W)))
    val floatFuncHit = WireDefault(VecInit.fill(2)(false.B))
    for(i <- 0 until nfch){
        if(i < 4) {
            // Integer Function Pipeline
            intFuncMap(i) := VecInit(io.func.map(_(i))).asUInt
            intFuncHit(i) := intFuncMap(i).orR
            val intFuncNotHitBeforeNum = BitAlign(if(i == 0) 0.U else PopCount(~(VecInit(intFuncHit.take(i)).asUInt)), log2Ceil(nfch))
            val portMap = Mux(intFuncHit(i), intFuncMap(i), aluInstructionIndexes(intFuncNotHitBeforeNum))
            io.bkePkg(i).valid := portMap.orR
            io.bkePkg(i).bits := Mux1H(portMap, io.ftePkg.map(_.bits))
        }else {
            // Floating Point Function Pipeline
            floatFuncMap(i-4) := VecInit(io.func.map(_(i))).asUInt
            floatFuncHit(i-4) := floatFuncMap(i-4).orR
            val floatFuncNotHitBeforeNum = BitAlign(if(i == 4) 0.U else PopCount(~(VecInit(floatFuncHit.take(i-4)).asUInt)), log2Ceil(nfch))
            val portMap = Mux(floatFuncHit(i-4), floatFuncMap(i-4), fpuInstructionIndexes(floatFuncNotHitBeforeNum))
            io.bkePkg(i).valid := portMap.orR
            io.bkePkg(i).bits := Mux1H(portMap, io.ftePkg.map(_.bits))
        }
    }
    io.ftePkg.foreach{ ftePkg => ftePkg.ready := bkePkgAllReday }
}