import spire.math.UInt
class FloatRegFile {
    private val rf = Array.fill(32)(0.0d)

    // 重定义()运算符，用于获取某个寄存器的值
    def apply(rd: Int): Double = {
        read(UInt(rd))
    }

    def read(rd: UInt): Double = {
        require(rd >= UInt(0) && rd < UInt(32), s"浮点寄存器编号必须在0-31范围内，当前值: $rd")
        rf(rd.toInt)
    }
    
    def write(rd: UInt, value: Double): Unit = {
        require(rd >= UInt(0) && rd < UInt(32), s"浮点寄存器编号必须在0-31范围内，当前值: $rd")
        rf(rd.toInt) = value
    }
    // // 打印所有寄存器的值，用于调试
    def dump(): Array[Double] = {
        rf
    }
}