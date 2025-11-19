import spire.math.UInt
import spire.math.SafeLong
class InstDecoder(rf: LogicRegFile, frf: FloatRegFile, mem: Memory, fetch: Fetch, ir: InstRecorder) {
    private def Bits(bits: UInt, hi: Int, lo: Int): UInt = {
        (bits >> lo) & ~(UInt(-1) << (hi - lo + 1))
    }
    private def signExtend(imm: UInt, width: Int): UInt = {
        if (Bits(imm, width - 1, width - 1) == UInt(1)) {
            imm | (UInt(-1) << width)
        } else {
            imm
        }
    }
    private def zeroExtend(imm: UInt, width: Int): UInt = {
        imm
    }
    private def executeRType(instruction: UInt, funct3: UInt, funct7: UInt): Unit = {
        val rd      = Bits(instruction, 11, 7)
        val rs1     = Bits(instruction, 19, 15)
        val rs2     = Bits(instruction, 24, 20)
        val opcode  = Bits(instruction, 6, 0)


        opcode.toInt match {
            case 0x33 => {
                val value1 = rf.read(rs1)
                val value2 = rf.read(rs2)
                funct7.toInt match {
                    case 0x00 => {
                        ir.addALUInsts(1)
                        val result = funct3.toInt match{
                            case 0x0 => value1 + value2             // add
                            case 0x1 => value1 << value2.toInt     // sll
                            case 0x2 => if(value1.toInt < value2.toInt) UInt(1) else UInt(0) // slt
                            case 0x3 => if(value1 < value2) UInt(1) else UInt(0) // sltu
                            case 0x4 => value1 ^ value2            // xor
                            case 0x5 => value1 >> value2.toInt     // srl
                            case 0x6 => value1 | value2            // or
                            case 0x7 => value1 & value2            // and
                            // case _ => throw new Exception("Invalid funct3 of funct7 = 0 in R-type")
                        }
                        rf.write(rd, result)
                    }

                    case 0x20 => {
                        ir.addALUInsts(1)
                        val result = funct3.toInt match{
                            case 0x0 => value1 - value2            // sub
                            case 0x5 => UInt(value1.toInt >> value2.toInt)  // sra
                            // case _ => throw new Exception("Invalid funct3 of funct7 = 0x20 in R-type")
                        }
                        rf.write(rd, result)
                    }
                    case 0x01 => {
                        val result = funct3.toInt match{
                            case 0x0 => {ir.addMulInsts(1); UInt(value1.toInt * value2.toInt)}                                      // mul
                            case 0x1 => {ir.addMulInsts(1); UInt(((BigInt(value1.toInt) * BigInt(value2.toInt)) >> 32).toLong)}    // mulh
                            case 0x2 => {ir.addMulInsts(1); UInt(((BigInt(value1.toInt) * value2.toLong) >> 32).toLong)}          // mulhsu
                            case 0x3 => {ir.addMulInsts(1); UInt(((value1.toLong * value2.toLong) >> 32).toLong)}                // mulhu
                            case 0x4 => {ir.addDivInsts(1); (if (value2 == UInt(0)) UInt(-1) else UInt(value1.toInt / value2.toInt))} // div
                            case 0x5 => {ir.addDivInsts(1); (if (value2 == UInt(0)) UInt(-1) else value1 / value2)} // divu
                            case 0x6 => {ir.addDivInsts(1); (if (value2 == UInt(0)) value1 else UInt(value1.toInt % value2.toInt))} // rem
                            case 0x7 => {ir.addDivInsts(1); (if (value2 == UInt(0)) value1 else value1 % value2)} // remu
                            // case _ => throw new Exception("Invalid funct3 of funct7 = 0x01 in R-type")
                        }
                        rf.write(rd, result)
                    }
                }
            }
            case 0x53 => {
                val value1 = frf.read(rs1)
                val value2 = frf.read(rs2)
                funct7.toInt match{
                    case 0x00 => {frf.write(rd, value1.toFloat + value2.toFloat)}
                    case 0x01 => {frf.write(rd, value1 + value2)}
                    case 0x04 => {frf.write(rd, value1.toFloat - value2.toFloat)}
                    case 0x05 => {frf.write(rd, value1 - value2)}
                    case 0x08 => {frf.write(rd, value1.toFloat * value2.toFloat)}
                    case 0x09 => {frf.write(rd, value1 * value2)}
                    case 0x0C => {frf.write(rd, value1.toFloat / value2.toFloat)}
                    case 0x0D => {frf.write(rd, value1 / value2)}
                    case 0x2C => {frf.write(rd, math.sqrt(value1.toFloat))}
                    case 0x2D => {frf.write(rd, math.sqrt(value1))}
                    case 0x10 => { 
                        funct3.toInt match{
                            case 0x0 => {
                                frf.write(rd, if(value2 < 0.0d) -math.abs(value1.toFloat) else math.abs(value1.toFloat))
                            }
                            case 0x1 => {
                                frf.write(rd, if(value2 > 0.0d) -math.abs(value1.toFloat) else math.abs(value1.toFloat))
                            }
                            case 0x2 => {
                                frf.write(rd, if(value2 < 0.0d) -math.abs(value1.toFloat) else math.abs(value1.toFloat))
                            }
                            case 0x3 => {
                                frf.write(rd, if((value2 < 0.0d) ^ (value1 < 0.0d)) -math.abs(value1.toFloat) else math.abs(value1.toFloat))
                            }
                        }
                    }
                    case 0x11 => {
                        funct3.toInt match{
                            case 0x0 => {
                                frf.write(rd, if(value2 < 0.0d) -math.abs(value1) else math.abs(value1))
                            }
                            case 0x1 => {
                                frf.write(rd, if(value2 > 0.0d) -math.abs(value1) else math.abs(value1))
                            }
                            case 0x2 => {
                                frf.write(rd, if(value2 < 0.0d) -math.abs(value1) else math.abs(value1))
                            }
                            case 0x3 => {
                                frf.write(rd, if((value2 < 0.0d) ^ (value1 < 0.0d)) -math.abs(value1) else math.abs(value1))
                            }
                        }
                    }
                    case 0x14 => {
                        funct3.toInt match{
                            case 0x0 => {
                                frf.write(rd, math.min(value1.toFloat, value2.toFloat))
                            }
                            case 0x1 => {
                                frf.write(rd, math.max(value1.toFloat, value2.toFloat))
                            }
                        }
                    }
                    case 0x15 => {
                        funct3.toInt match{
                            case 0x0 => {
                                frf.write(rd, math.min(value1, value2))
                            }
                            case 0x1 => {
                                frf.write(rd, math.max(value1, value2))
                            }
                        }
                    }
                    case 0x60 => {
                        rs2.toInt match{
                            case 0x00 => { rf.write(rd, UInt(value1.toFloat.toInt))}
                            case 0x01 => { rf.write(rd, UInt(value1.toFloat.toLong & 0xFFFFFFFFL))}
                        }
                    }
                    case 0x61 => {
                        rs2.toInt match{
                            case 0x00 => { rf.write(rd, UInt(value1.toInt))}
                            case 0x01 => { rf.write(rd, UInt(value1.toLong & 0xFFFFFFFFL))}
                        }
                    }
                    case 0x40 => {
                        rs2.toInt match{
                            case 0x00 => { frf.write(rd, value1.toDouble)}
                            case 0x01 => { frf.write(rd, value1.toFloat)}
                        }
                    }
                    case 0x70 => {
                        rs2.toInt match{
                            case 0x00 => { funct3.toInt match { 
                                case 0x0 => UInt(java.lang.Float.floatToIntBits(value1.toFloat))
                                case 0x1 => {
                                    // fclass.s - 浮点分类指令
                                    val floatValue = value1.toFloat
                                    val bits = java.lang.Float.floatToIntBits(floatValue)
                                    val sign = (bits >>> 31) & 1
                                    val exponent = (bits >>> 23) & 0xFF
                                    val mantissa = bits & 0x7FFFFF
                                    
                                    val classification = if (exponent == 0xFF) {
                                        // 指数全1的情况
                                        if (mantissa == 0) {
                                            // 无穷大
                                            if (sign == 1) 1 << 0 else 1 << 7  // 负无穷或正无穷
                                        } else {
                                            // NaN
                                            if ((mantissa & 0x400000) != 0) 1 << 9 else 1 << 8  // 安静NaN或信号NaN
                                        }
                                    } else if (exponent == 0) {
                                        // 指数全0的情况
                                        if (mantissa == 0) {
                                            // 零
                                            if (sign == 1) 1 << 3 else 1 << 4  // 负零或正零
                                        } else {
                                            // 非规格化数
                                            if (sign == 1) 1 << 2 else 1 << 5  // 负非规格化数或正非规格化数
                                        }
                                    } else {
                                        // 规格化数
                                        if (sign == 1) 1 << 1 else 1 << 6  // 负规格化数或正规格化数
                                    }
                                    
                                    rf.write(rd, UInt(classification))
                                }
                            }}
                        }
                    }
                    case 0x71 => {
                        rs2.toInt match{
                            case 0x00 => { funct3.toInt match { 
                                case 0x0 => UInt(java.lang.Double.doubleToLongBits(value1.toDouble))
                                case 0x1 => {
                                    // fclass.d - 双精度浮点分类指令
                                    val doubleValue = value1.toDouble
                                    val bits = java.lang.Double.doubleToLongBits(doubleValue)
                                    val sign = (bits >>> 63) & 1L
                                    val exponent = ((bits >>> 52) & 0x7FFL).toInt
                                    val mantissa = bits & 0xFFFFFFFFFFFFFL
                                    
                                    val classification = if (exponent == 0x7FF) {
                                        // 指数全1的情况
                                        if (mantissa == 0L) {
                                            // 无穷大
                                            if (sign == 1L) 1 << 0 else 1 << 7  // 负无穷或正无穷
                                        } else {
                                            // NaN
                                            if ((mantissa & 0x8000000000000L) != 0L) 1 << 9 else 1 << 8  // 安静NaN或信号NaN
                                        }
                                    } else if (exponent == 0) {
                                        // 指数全0的情况
                                        if (mantissa == 0L) {
                                            // 零
                                            if (sign == 1L) 1 << 3 else 1 << 4  // 负零或正零
                                        } else {
                                            // 非规格化数
                                            if (sign == 1L) 1 << 2 else 1 << 5  // 负非规格化数或正非规格化数
                                        }
                                    } else {
                                        // 规格化数
                                        if (sign == 1L) 1 << 1 else 1 << 6  // 负规格化数或正规格化数
                                    }
                                    
                                    rf.write(rd, UInt(classification))
                                }
                            }}
                        }
                    }
                    case 0x50 => {
                        funct3.toInt match{
                            case 0x2 => {rf.write(rd, UInt(if(value1.toFloat == value2.toFloat) 1 else 0))}
                            case 0x1 => {rf.write(rd, UInt(if(value1.toFloat < value2.toFloat) 1 else 0))}
                            case 0x0 => {rf.write(rd, UInt(if(value1.toFloat <= value2.toFloat) 1 else 0))}
                        }
                    }
                    case 0x51 => {
                        funct3.toInt match{
                            case 0x2 => {rf.write(rd, UInt(if(value1 == value2) 1 else 0))}
                            case 0x1 => {rf.write(rd, UInt(if(value1 < value2) 1 else 0))}
                            case 0x0 => {rf.write(rd, UInt(if(value1 <= value2) 1 else 0))}
                        }
                    }
                    case 0x68 => {
                        val rvalue = rf.read(rs1)
                        rs2.toInt match {
                            case 0x00 => { frf.write(rd, rvalue.toFloat)}
                            case 0x01 => { frf.write(rd, (rvalue.toLong & 0xFFFFFFFFL).toFloat)}
                        }
                    }
                    case 0x69 => {
                        val rvalue = frf.read(rs1)
                        rs2.toInt match {
                            case 0x00 => { frf.write(rd, rvalue.toDouble)}
                            case 0x01 => { frf.write(rd, (rvalue.toLong & 0xFFFFFFFFL).toDouble)}
                        }
                    }
                    case 0x78 => {
                        val rvalue = frf.read(rs1)
                        rs2.toInt match {
                            case 0x00 => { funct3.toInt match { case 0x0 => frf.write(rd, java.lang.Float.intBitsToFloat(rvalue.toInt))}}
                        }
                    }
                }
            }
        }
        
        fetch.setPC(fetch.getPC() + UInt(4))
    }
    private def executeIType(instruction: UInt, funct3: UInt): Unit = {
        val rd      = Bits(instruction, 11, 7)
        val rs1     = Bits(instruction, 19, 15)
        val imm     = signExtend(Bits(instruction, 31, 20), 12)
        val opcode  = Bits(instruction, 6, 0)
        val pc      = fetch.getPC()
        val value1  = rf.read(rs1)
        opcode.toInt match {
            case 0x13 => {
                ir.addALUInsts(1)
                fetch.setPC(pc + UInt(4))
                val result = funct3.toInt match{
                    case 0x0 => value1 + imm // addi
                    case 0x1 => value1 << imm.toInt // slli
                    case 0x2 => if(value1.toInt < imm.toInt) UInt(1) else UInt(0) // slti
                    case 0x3 => if(value1 < imm) UInt(1) else UInt(0) // sltiu
                    case 0x4 => value1 ^ imm // xori
                    case 0x5 => if((instruction.toInt & 0x40000000) == 0) value1 >> (imm.toInt & 0x1F) else UInt(value1.toInt >> (imm.toInt & 0x1F)) // srli & srai
                    case 0x6 => value1 | imm // ori
                    case 0x7 => value1 & imm // andi
                    // case _ => throw new Exception("Invalid funct3 of I-type")
                }
                rf.write(rd, result)
            }
            case 0x03 => {
                ir.addLoadInsts(1)
                fetch.setPC(pc + UInt(4))
                val result = mem.read(value1 + imm, funct3.toInt)
                rf.write(rd, UInt(result.toLong))
            }
            case 0x67 => {
                ir.addBranchInsts(1)
                fetch.setPC(value1 + imm)
                val result = pc + UInt(4)
                rf.write(rd, result)
            }
            case 0x07 => { 
                // ir.addALUInsts(1)
                fetch.setPC(pc + UInt(4))
                val result = java.lang.Double.longBitsToDouble(mem.read(value1 + imm, funct3.toInt).toLong)
                frf.write(rd, result)
            }
        }
        
    }
    private def executeBType(instruction: UInt, funct3: UInt): Unit = {
        val rs1     = Bits(instruction, 19, 15)
        val rs2     = Bits(instruction, 24, 20)
        val imm     = signExtend(Bits(instruction, 31, 31) << 12 | (Bits(instruction, 7, 7) << 11) | (Bits(instruction, 30, 25) << 5) | (Bits(instruction, 11, 8) << 1), 13)
        val opcode  = Bits(instruction, 6, 0)
        val pc      = fetch.getPC()

        val value1  = rf.read(rs1)
        val value2  = rf.read(rs2)
        val npc     = (opcode.toInt match {
            case 0x63 => {
                ir.addBranchInsts(1)
                funct3.toInt match {
                    case 0x0 => if (value1 == value2) pc + imm else pc + UInt(4)
                    case 0x1 => if (value1 != value2) pc + imm else pc + UInt(4)
                    case 0x4 => if (value1.toInt < value2.toInt) pc + imm else pc + UInt(4)
                    case 0x5 => if (value1.toInt >= value2.toInt) pc + imm else pc + UInt(4)
                    case 0x6 => if (value1 < value2) pc + imm else pc + UInt(4)
                    case 0x7 => if (value1 >= value2) pc + imm else pc + UInt(4)
                }
            }
        })
        fetch.setPC(npc)
    }
    private def executeSType(instruction: UInt, funct3: UInt): Unit = {
        val rs1     = Bits(instruction, 19, 15)
        val rs2     = Bits(instruction, 24, 20)
        val imm     = signExtend(Bits(instruction, 31, 25) << 5 | Bits(instruction, 11, 7), 12)
        val value1  = rf.read(rs1)
        val opcode  = Bits(instruction, 6, 0)
        opcode.toInt match {
            case 0x23 => { mem.write(value1 + imm, rf.read(rs2).toBigInt, funct3.toInt) }
            case 0x27 => { mem.write(value1 + imm, BigInt(java.lang.Double.doubleToLongBits(frf.read(rs2).toDouble)), funct3.toInt) }
        }
        ir.addStoreInsts(1)
        fetch.setPC(fetch.getPC() + UInt(4))
    }
    private def executeUType(instruction: UInt): Unit = {
        val rd      = Bits(instruction, 11, 7)
        val imm     = Bits(instruction, 31, 12) << 12
        val opcode  = Bits(instruction, 6, 0)
        val pc      = fetch.getPC()
        val result  = opcode.toInt match {
            case 0x37 => { imm }
            case 0x17 => { pc + imm }
        }
        ir.addALUInsts(1)
        rf.write(rd, result)
        fetch.setPC(pc + UInt(4))
    }
    private def executeJType(instruction: UInt): Unit = {
        val rd      = Bits(instruction, 11, 7)
        val rs1     = Bits(instruction, 19, 15)
        val imm     = signExtend(Bits(instruction, 31, 31) << 20 | (Bits(instruction, 19, 12) << 12) | (Bits(instruction, 20, 20) << 11) | (Bits(instruction, 30, 21) << 1), 21)
        val opcode  = Bits(instruction, 6, 0)
        val pc      = fetch.getPC()
        val value1  = rf.read(rs1)
        val result  = opcode.toInt match {
            case 0x6F => { 
                ir.addBranchInsts(1)
                fetch.setPC(pc + imm)
                pc + UInt(4) 
            }
        }
        rf.write(rd, result)
    }

    private def executeR4Type(instruction: UInt, funct3: UInt, funct2: UInt): Unit = {
        val rd      = Bits(instruction, 11, 7)
        val rs1     = Bits(instruction, 19, 15)
        val rs2     = Bits(instruction, 24, 20)
        val rs3     = Bits(instruction, 31, 27)
        val opcode  = Bits(instruction, 6, 0)
        val value1  = frf.read(rs1)
        val value2  = frf.read(rs2)
        val value3  = frf.read(rs3)
        val result  = opcode.toInt match {
            case 0x43 => { if(funct2.toInt == 0x0) value1.toFloat * value2.toFloat + value3.toFloat else value1 * value2 + value3 }
            case 0x47 => { if(funct2.toInt == 0x0) value1.toFloat * value2.toFloat - value3.toFloat else value1 * value2 - value3 }
            case 0x4B => { if(funct2.toInt == 0x0) -value1.toFloat * value2.toFloat - value3.toFloat else -value1 * value2 - value3 }
            case 0x4F => { if(funct2.toInt == 0x0) -value1.toFloat * value2.toFloat + value3.toFloat else -value1 * value2 + value3 }
        }
        frf.write(rd, result)
        fetch.setPC(fetch.getPC() + UInt(4))
    }
    def decodeAndExecute(instruction: UInt): Unit = {
        val opcode = Bits(instruction, 6, 0)
        val funct2 = Bits(instruction, 26, 25)
        val funct3 = Bits(instruction, 14, 12)
        val funct7 = Bits(instruction, 31, 25)
        opcode.toInt match {
            case 0x37 => { executeUType(instruction) }
            case 0x17 => { executeUType(instruction) }
            case 0x6F => { executeJType(instruction) }
            case 0x67 => { executeIType(instruction, funct3) }
            case 0x07 => { executeIType(instruction, funct3) }
            case 0x63 => { executeBType(instruction, funct3) }
            case 0x03 => { executeIType(instruction, funct3) }
            case 0x23 => { executeSType(instruction, funct3) }
            case 0x27 => { executeSType(instruction, funct3) }
            case 0x13 => { executeIType(instruction, funct3) }
            case 0x33 => { executeRType(instruction, funct3, funct7) }
        }
    }
}