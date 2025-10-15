import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Fetch._
import ZirconConfig.Dispatch._

class PredictInfo extends Bundle {
    val offset     = UInt(32.W)
    val jumpEn     = Bool()
    val vld        = Bool()
    val pcPlus4    = UInt(32.W)
}
class RegisterInfo extends Bundle {
    val rdVld = Bool()
    val rd    = UInt(5.W)
    val rj    = UInt(5.W)
    val rk    = UInt(5.W)
}
class FrontendPackage extends Bundle {
    val valid      = Bool()
    val pc         = UInt(32.W)
    val predInfo   = new PredictInfo()
    val inst       = UInt(32.W)
    val op         = UInt(9.W)
    val imm        = UInt(32.W)
    val func       = UInt((nfuncUnit).W)
    val rinfo      = new RegisterInfo()
}

class BackendPackage extends Bundle {
    val valid      = Bool()
    val pc         = UInt(32.W)
    val predOffset = UInt(32.W)
    val rdVld      = Bool()
    val rj         = UInt(5.W)
    val rk         = UInt(5.W)
    val rd         = UInt(5.W)
    val op         = UInt(9.W)
    val imm        = UInt(32.W)


    val src1       = UInt(32.W)
    val src2       = UInt(32.W)
    val rfWdata    = UInt(32.W)
    val jumpEn     = Bool()
    val predFail   = Bool()
    val result     = UInt(32.W)
    
    def apply(fte: FrontendPackage): BackendPackage = {
        val bke = WireDefault(0.U.asTypeOf(new BackendPackage))
        bke.valid      := fte.valid
        bke.pc         := fte.pc
        bke.rdVld      := fte.rinfo.rdVld
        bke.op         := fte.op(6, 0)
        bke.imm        := fte.imm
        bke.rj         := fte.rinfo.rj
        bke.rk         := fte.rinfo.rk
        bke.rd         := fte.rinfo.rd
        bke
    }
}
