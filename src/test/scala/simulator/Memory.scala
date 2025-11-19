
import spire.math.UInt
import scala.collection.mutable.{LongMap}

class Memory {
  // 使用LongMap替代HashMap，因为UInt的哈希操作较慢，LongMap对于Long类型的键有更好的性能
  private val mem = new LongMap[Byte]()
  
  // RISCV访问类型常量
  object AccessType {
    val BYTE = 0       // 000: 字节读取，符号扩展 (lb)
    val HALF = 1       // 001: 半字读取，符号扩展 (lh)
    val WORD = 2       // 010: 字读取 (lw)
    val DOUBLE = 3     // 011: 双字读取 (ld)
    val BYTEU = 4     // 100: 字节读取，无符号扩展 (lbu)
    val HALFU = 5     // 101: 半字读取，无符号扩展 (lhu)
  
  }
  
  // 读内存 - 根据funct3提供的访问类型返回适当扩展的值
  def read(addr: UInt, accessType: Int): BigInt = {
    accessType match {
      case AccessType.BYTE => signExtend(readByte(addr), 8)
      case AccessType.HALF => signExtend(readHalf(addr), 16)
      case AccessType.WORD => readWord(addr)
      case AccessType.BYTEU => zeroExtend(readByte(addr), 8)
      case AccessType.HALFU => zeroExtend(readHalf(addr), 16)
      case _ => throw new IllegalArgumentException(s"不支持的访问类型: $accessType")
    }
  }
  
  // 写内存 - 根据funct3提供的访问类型写入适当大小的数据
  def write(addr: UInt, data: BigInt, accessType: Int): Unit = {
    accessType match {
      case AccessType.BYTE | AccessType.BYTEU => 
        writeByte(addr, data & 0xFF)
      case AccessType.HALF | AccessType.HALFU => 
        writeHalf(addr, data & 0xFFFF)
      case AccessType.WORD => 
        writeWord(addr, data & 0xFFFFFFFF)
      case AccessType.DOUBLE => 
        writeDouble(addr, data)
      case _ => 
        throw new IllegalArgumentException(s"不支持的访问类型: $accessType")
    }
  }
  
  // 读取一个字节
  private def readByte(addr: UInt): BigInt = {
    val value = mem.getOrElse(addr.toLong, 0.toByte)
    BigInt(value & 0xFF)
  }
  
  // 读取半字 (16位)
  private def readHalf(addr: UInt): BigInt = {
    require((addr.toLong & 1) == 0, s"半字访问必须2字节对齐，地址: $addr")
    val low = readByte(addr)
    val high = readByte(addr + UInt(1))
    (high << 8) | low
  }
  
  // 读取一个字 (32位)
  private def readWord(addr: UInt): BigInt = {
    require((addr.toLong & 3) == 0, s"字访问必须4字节对齐，地址: $addr")
    val byte0 = readByte(addr)
    val byte1 = readByte(addr + UInt(1))
    val byte2 = readByte(addr + UInt(2))
    val byte3 = readByte(addr + UInt(3))
    (byte3 << 24) | (byte2 << 16) | (byte1 << 8) | byte0
  }
  // 读取一个双字（64）
  private def readDouble(addr: UInt): BigInt = {
    require((addr.toLong & 3) == 0, s"字访问必须4字节对齐，地址: $addr")
    val byte0 = readByte(addr).toLong
    val byte1 = readByte(addr + UInt(1)).toLong
    val byte2 = readByte(addr + UInt(2)).toLong
    val byte3 = readByte(addr + UInt(3)).toLong
    val byte4 = readByte(addr + UInt(4)).toLong
    val byte5 = readByte(addr + UInt(5)).toLong
    val byte6 = readByte(addr + UInt(6)).toLong
    val byte7 = readByte(addr + UInt(7)).toLong
    (byte7 << 56) | (byte6 << 48) | (byte5 << 40) | (byte4 << 32) | (byte3 << 24) | (byte2 << 16) | (byte1 << 8) | byte0
  }
  
  // 写入一个字节
  private def writeByte(addr: UInt, data: BigInt): Unit = {
    mem(addr.toLong) = (data.toLong & 0xFF).toByte
  }
  
  // 写入半字 (16位)
  private def writeHalf(addr: UInt, data: BigInt): Unit = {
    require((addr.toLong & 1) == 0, s"半字访问必须2字节对齐，地址: ${addr.toInt.toHexString}")
    writeByte(addr, data & BigInt(0xFF))
    writeByte(addr + UInt(1), (data >> 8) & BigInt(0xFF))
  }
  
  // 写入一个字 (32位)
  private def writeWord(addr: UInt, data: BigInt): Unit = {
    require((addr.toLong & 3) == 0, s"字访问必须4字节对齐，地址: ${addr.toInt.toHexString}")
    writeByte(addr, data & BigInt(0xFF))
    writeByte(addr + UInt(1), (data >> 8) & BigInt(0xFF))
    writeByte(addr + UInt(2), (data >> 16) & BigInt(0xFF))
    writeByte(addr + UInt(3), (data >> 24) & BigInt(0xFF))
  }

  // 写入一个双字（64）
  private def writeDouble(addr: UInt, data: BigInt): Unit = {
    require((addr.toLong & 3) == 0, s"字访问必须4字节对齐，地址: ${addr.toInt.toHexString}")
    writeByte(addr, data & BigInt(0xFF))
    writeByte(addr + UInt(1), (data >> 8) & BigInt(0xFF))
    writeByte(addr + UInt(2), (data >> 16) & BigInt(0xFF))
    writeByte(addr + UInt(3), (data >> 24) & BigInt(0xFF))
  }
  // 符号扩展
  def signExtend(value: BigInt, originalBits: Int): BigInt = {
    val signBit = (value >> (originalBits - 1)) & BigInt(1)
    if (signBit == BigInt(1)) {
      // 如果符号位为1，则扩展为1
      val mask = BigInt((1L << 32) - 1) - BigInt((1L << originalBits) - 1)
      value | mask
    } else {
      // 如果符号位为0，则保持不变
      value
    }
  }
  
  // 零扩展
  def zeroExtend(value: BigInt, originalBits: Int): BigInt = {
    // 无符号扩展只需确保高位为0
    value & BigInt((1L << originalBits) - 1)
  }
  
  // 清空内存
  def reset(): Unit = {
    mem.clear()
  }
  
  // 从文件加载内存数据（可用于加载程序）
  def loadFromFile(filename: String, baseAddr: UInt): Unit = {
    if(filename == "" || filename == null) {
        // println("没有提供镜像文件路径，使用默认镜像")
        writeWord(baseAddr, BigInt(0x80000000))
        return
    }
    import java.nio.file.{Files, Paths}
    val bytes = Files.readAllBytes(Paths.get(filename))
    bytes.zipWithIndex.foreach { case (byte, i) =>
      writeByte(baseAddr + UInt(i), BigInt(byte & 0xFF))
    }
  }
  
//   // 调试：打印内存区域的内容
//   def dump(startAddr: UInt, length: Int): Unit = {
//     println(s"内存转储 从 $startAddr 开始, 长度 $length 字节:")
//     for (i <- 0 until length) {
//       val addr = startAddr + UInt(i)
//       val value = mem.getOrElse(addr, 0.toByte)
//       print(f"${value & 0xFF}%02X ")
//       if ((i + 1) % 16 == 0) println()
//     }
//     println()
//   }
}

