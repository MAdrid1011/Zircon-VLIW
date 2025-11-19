# Zircon-VLIW 处理器设计

## 概述

Zircon-VLIW（以下简称Zircon）是一款基于RISC-V的VLIW处理器，目标是高效处理DSP任务。处理器使用8取指、8译码、8发射、8提交的结构来实现，能够处理由VLIW编译器生成的RISC-V imf程序。

在原型机阶段，Zircon使用哈佛架构，假设拥有单周期100%命中无限大Cache——即访存行为（包括取指、加载、存储）完全由软件环境Emulator在单周期内完成。

![RProject.drawio](./arch.assets/RProject.drawio.svg)

Zircon-VLIW原型机的设计模块关系如下：

* Frontend

  * NPC（下一PC生成）

  * Decoder *8（8路译码器）
  * FPRegfile（浮点寄存器堆）
  * GPRegfile（通用寄存器堆）

* Backend

  * FDiv-FPU Pipeline
  * ALU-FPU Pipeline * 2
  * ALU-iMD Pipeline * 2（ALU-整数乘除法）
  * ALU-LSU Pipeline * 2（ALU-访存）
  * ALU-Branch Pipeline * 2

* Hazard

## 指令传递范式

在Zircon中，指令以InstructionPackage类为基本单元，该类中包含了一条指令从取指到写回可能携带的所有信息，包括指令码、读写数据等。这个包会随着流水线向后传递，并在编译时借助Firrtl的死代码删除来裁剪掉不会在后续流水级使用的成员。

InstructionPackage的定义如下：

```scala
class InstructionPackage extends Bundle {
    /* IF Stage */
    val pc        = UInt(32.W)
    val inst      = UInt(32.W)
    /* ID Stage */
    // regfile
    val rs1       = UInt(6.W) // the highest bit to judge if it is float
    val rs2       = UInt(6.W) // the highest bit to judge if it is float
    val rs3       = UInt(6.W) // the highest bit to judge if it is float
    val rd        = UInt(6.W) // the highest bit to judge if it is float
    val rdValid   = Bool()
    val rs1Data   = UInt(32.W)
    val rs2Data   = UInt(32.W)
    val rs3Data   = UInt(32.W)
    // operation
    val op        = UInt(7.W) // [3:0]: operation; [4]: is branch or load or muldiv [5]: is store  [6]: is float (not fdiv) 
    val imm       = UInt(32.W)
    val src1Sel   = UInt(1.W) // 0: rs1; 1: pc; 
    val src2Sel   = UInt(1.W) // 0: rs2; 1: imm;
    /* EX Stage */
    val aluResult = UInt(32.W)
    val fpuResult = UInt(32.W)
    val branchTgt = UInt(32.W)
    val predFail  = Bool()
    val memResult = UInt(32.W)
    /* WB Stage */
    val rfWdata   = UInt(32.W)

    def IFUpdate(pc: UInt, inst: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.pc     := pc
        instPkg.inst   := inst
        instPkg
    }
    def IDUpdate(rs1: UInt, rs2: UInt, rs3: UInt, rd: UInt, rdValid: Bool, op: UInt, imm: UInt, src1Sel: UInt, src2Sel: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.rs1       := rs1
        instPkg.rs2       := rs2
        instPkg.rs3       := rs3
        instPkg.rd        := rd
        instPkg.rdValid   := rdValid
        instPkg.op        := op
        instPkg.imm       := imm
        instPkg.src1Sel   := src1Sel
        instPkg.src2Sel   := src2Sel
        instPkg
    }
    def IDUpdate(rs1Data: UInt, rs2Data: UInt, rs3Data: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.rs1Data   := rs1Data
        instPkg.rs2Data   := rs2Data
        instPkg.rs3Data   := rs3Data
        instPkg
    }
    def EX1Update(aluResult: UInt, branchTgt: UInt, predFail: Bool): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.aluResult := aluResult
        instPkg.branchTgt := branchTgt
        instPkg.predFail  := predFail
        instPkg
    }
    def EX2Update(memResult: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.memResult := memResult
        instPkg
    }
    def EX3Update(fpuResult: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.fpuResult := fpuResult
        instPkg
    }
    def WBUpdate(rfWdata: UInt): InstructionPackage = {
        val instPkg = WireDefault(this)
        instPkg.rfWdata := rfWdata
        instPkg
    }
}
```

其中，在最后的几个方法，是为了能够在流水线的不同流水级，从不同的模块中将成员逐渐“组装”到InstructionPackage中。除了Decoder会在元件内组装之外，其余的方法都是在Frontend.scala和Backend.scala按照流水级进行调用。



## Frontend

前端的代码目录结构如下：

```
Frontend/
├── Frontend.scala
├── Fetch/
│   └──NPC.scala
├── Decode/
│   ├── Decoder.scala
│   └── RVISA.scala
└── Regfile/
    └── Regfile.scala
```

### Frontend

前端由两个流水级组成：IF（取指）和ID（指令译码）。其中，NPC和PC寄存器位于IF段，而Decoder们和两个例化的寄存器堆位于ID段，流水级之间使用实现段间寄存器分割。除此之外，取指行为在IF端发起，当周期仿真环境就会根据PC值给回8条地址上连续的指令。

FrontendIO有3套接口：

* FrontendMemIO：将PC值发送给仿真环境，仿真环境一次给回以当前PC为起始地址的连续8条指令
* FrontendBackendIO：将ID段组装好的8个InstPkg发送给后端，接收后端来的重定向目标地址，并接收后端来的写回请求。
* FrontendHazardIO：Hazard给出的flush和stall信号，用来控制ShiftRegister的流动和NPC的生成

当Hazard给出flush信号时，IF-ID段间寄存器应当被冲刷，NPC应当重新基于后端的重定向目标地址来重定向PC。当Hazard给出stall信号时，IF-ID段间寄存器应当被阻塞，NPC应当保持PC不变

### NPC

NPC模块负责根据流水线的执行情况来计算下一个PC并实现PC的重定向，NPCIO拥有3套接口：

* NPCHazardIO：Hazard的冲刷和阻塞信号
* NPCBackendIO：后端计算单元给出的PC重定向地址
* NPCFrontendIO：前端中的PC值，并向前端发送npc（下一个PC值）

### Decoder

Decoder将指令进行译码，转换为InstructionPackage中的部分字段。由于每条流水线只能到来部分固定类型的指令，因此Decoder具有模板化设计（通过输入参数控制决定能译码哪些类型的指令）。8条流水线的功能部件如表所示：

| 流水线编号 | 0    | 1             | 2            | 3        | 4        | 5    | 6    | 7      |
| ---------- | ---- | ------------- | ------------ | -------- | -------- | ---- | ---- | ------ |
| 部件1      | FDiv | ALU           | ALU          | ALU      | ALU      | ALU  | ALU  | ALU    |
| 部件2      | FPU  | FPU (FPToInt) | FPU(IntToFP) | iMul/Div | iMul/Div | LSU  | LSU  | Branch |

具体部件的功能后文讲解。因此，我们为每一个功能部件可以执行的指令都创建了一个译码表（即指令码到操作微码的映射）。在执行时，Decoder会查询自己支持的所有译码表，并确定当前指令所需要的操作微码，并将其打包到InstPkgOut中并输出。

Decider只有一套IO：

* DecoderIO：输入inst，输出InstPkgOut

Decoder基于RVISA.scala中使用BitPat定义的指令集编码进行工作，具体操作微码编码在Config.scala中。

译码器实现有基础细节：

* 对于寄存器编号，指令集中是5位，但我们扩展了1位，如果是GPR，那么最高位为0；如果是FPR，那么最高位是1。
* 对于rd编号为0且是GPR的情况，rdValid将会被设置为false.B。这是为了简化Regfile的写优先设计：
  * 如果GPR写0，那么这个写入值不应该被写优先前递，因为0号寄存器不管怎么读都是0。所以当初始化为0时，GPR的写前递、读逻辑都不需要特判地址为0的问题
  * FPR并没有“0号寄存器一定是0”的规定

### Regfile

写优先、长度为32、宽度为32位的读、写端口可以参数化的寄存器堆模板，由Frontend例化为gpr和fpr。由于RISC-V寄存器编号在固定位置，因此读地址无需经过译码就可以直接由Frontend连接到Regfile中

前端并不是8个通道都需要同时读GPR和FPR，后端也并不是每一个流水线都有可能写回GPR和FPR，具体连线分配关系如下表所示：

| 流水线编号 | 0    | 1    | 2    | 3    | 4    | 5    | 6    | 7    |
| ---------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| 读GPR口    | 0    | 2    | 2    | 2    | 2    | 2    | 2    | 2    |
| 读FPR口    | 3    | 3    | 3    | 0    | 0    | 0    | 0    | 0    |
| 写GPR口    | 0    | 1    | 1    | 1    | 1    | 1    | 1    | 1    |
| 写FPR口    | 1    | 1    | 1    | 0    | 0    | 0    | 0    | 0    |

因此，GPR由14个读口和7个写口，FPR有9个读口和3个写口，每个寄存器堆内，所有读口对所有写口都需要判断写优先。



