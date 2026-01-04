# VLIW 反汇编分析工具

一个用于分析 Zircon-VLIW 处理器反汇编代码的 Python 工具，评估指令打包优化潜力和填充指令开销。

## 功能特性

### 1. 原始包统计分析
- 统计 VLIW 包数量和指令分布
- 识别有效指令和填充指令
- 计算平均每包指令密度

### 2. 填充指令分析
- 识别包前、包后、包中填充指令
- 计算可删除的填充指令数量
- 评估程序大小缩减潜力

### 3. 一层依赖重打包优化
- 分析指令间的数据依赖关系
- 允许一层依赖的 VLIW 重打包
- 对比优化前后的包数量和指令密度

## 安装

```bash
cd VLIW_PACK_Analyzer
# 无需安装外部依赖，仅需 Python 3.8+
```

## 使用方法

### 基本用法

```bash
# 分析单个反汇编文件
python main.py ../VLIW_PACK/FFT/build/FFT-riscv32.txt

# 指定输出文件
python main.py ../VLIW_PACK/FFT/build/FFT-riscv32.txt --output report.txt

# 详细输出模式
python main.py ../VLIW_PACK/FFT/build/FFT-riscv32.txt --verbose
```

### 批量分析

```bash
# 分析多个文件
python main.py ../VLIW_PACK/functest/build/*.txt --batch

# 生成 JSON 格式报告
python main.py ../VLIW_PACK/FFT/build/FFT-riscv32.txt --format json --output report.json
```

### 命令行参数

```
usage: main.py [-h] [--output OUTPUT] [--format {text,json}] 
               [--verbose] [--batch] input_file [input_file ...]

VLIW 反汇编分析工具

positional arguments:
  input_file            反汇编文件路径

optional arguments:
  -h, --help            显示帮助信息
  --output OUTPUT, -o OUTPUT
                        输出报告文件路径（默认：标准输出）
  --format {text,json}, -f {text,json}
                        报告格式（默认：text）
  --verbose, -v         显示详细输出
  --batch, -b           批量处理模式
```

## 输出报告示例

```
=== VLIW 反汇编分析报告 ===

文件：FFT-riscv32.txt
分析时间：2026-01-04 12:00:00

--- 原始包统计 ---
总包数：274
总指令数：2192 (274 × 8)
有效指令数：1245 (56.8%)
填充指令数：947 (43.2%)
  - nop (0x00000013): 623 条
  - feq.s zero (0xa0002053): 324 条
平均每包有效指令：4.54

--- 填充指令分析 ---
包前填充：156 条 (16.5%)
包后填充：189 条 (20.0%)
包中填充：602 条 (63.5%)
可删除填充：345 条 (包前 + 包后)
程序大小：8768 字节 (2192 × 4)
优化后大小：7388 字节
程序大小缩减：1380 字节 (15.7%)

--- 一层依赖重打包分析 ---
优化后包数：198
包数量减少：76 (27.7%)
有效指令数：1245 (不变)
平均每包有效指令：6.29
指令密度提升：38.5%

单周期 ALU 指令：892 (71.6%)
可形成一层依赖对：156 对
成功合并到同一包：134 对 (85.9%)

--- 详细统计 ---
指令类型分布：
  ALU 指令：892 (71.6%)
    - 算术运算：456
    - 逻辑运算：234
    - 移位运算：123
    - 比较运算：79
  Load/Store：234 (18.8%)
  分支跳转：67 (5.4%)
  乘除法：32 (2.6%)
  浮点运算：20 (1.6%)

依赖关系统计：
  无依赖指令：567 (45.5%)
  有依赖指令：678 (54.5%)
    - 一层依赖：312 (46.0%)
    - 多层依赖：366 (54.0%)

--- 优化建议 ---
1. 编译器可通过允许一层依赖打包减少 27.7% 的包数量
2. 删除包前/包后填充可节省 15.7% 的代码空间
3. 当前指令密度较低（4.54/8），有较大优化空间
4. 85.9% 的一层依赖对可成功合并，优化效果显著

=== 分析完成 ===
```

## 核心概念

### VLIW 包结构

Zircon-VLIW 处理器采用 8 取指、8 译码、8 发射的并行结构，每个 VLIW 包包含 8 条指令。

```
VLIW Package (8 instructions)
┌─────────────────────────────┐
│ Instruction 0               │
│ Instruction 1               │
│ Instruction 2               │
│ Instruction 3               │
│ Instruction 4               │
│ Instruction 5               │
│ Instruction 6               │
│ Instruction 7               │
└─────────────────────────────┘
```

### 填充指令

为了对齐 VLIW 包边界，编译器会插入填充指令：

- **nop** (`0x00000013`)：空操作
- **feq.s zero, ft0, ft0** (`0xa0002053`)：无效的浮点比较

填充指令分为三类：
- **包前填充**：包开头的连续填充指令
- **包后填充**：包末尾的连续填充指令
- **包中填充**：夹在有效指令之间的填充（可能用于对齐）

### 一层依赖

**定义**：指令 B 依赖指令 A 的结果，且 A 和 B 都是单周期算术指令。

**示例**：
```assembly
add  a0, a1, a2    # A: a0 = a1 + a2 (单周期 ALU)
addi a3, a0, 1     # B: a3 = a0 + 1 (单周期 ALU，依赖 A 的 a0)
```

在传统编译器中，A 和 B 必须在不同的 VLIW 包中。但如果允许一层依赖，它们可以在同一个包中，从而减少包数量。

**约束**：
- Producer 和 Consumer 都必须是单周期 ALU 指令
- 不包括乘除法、Load/Store、浮点运算等多周期指令

### 单周期算术指令

基于 Zircon-VLIW 架构，以下指令在 EX1 阶段完成，结果可在 EX2 阶段前递：

- **算术运算**：`add`, `addi`, `sub`
- **逻辑运算**：`and`, `andi`, `or`, `ori`, `xor`, `xori`
- **移位运算**：`sll`, `slli`, `srl`, `srli`, `sra`, `srai`
- **比较运算**：`slt`, `slti`, `sltu`, `sltiu`
- **立即数加载**：`lui`, `auipc`

**排除**：
- 乘除法：`mul`, `div`, `rem` 等（需要 3 级流水）
- Load/Store：`lw`, `sw` 等（需要访存）
- 浮点运算：`fadd.s`, `fmul.s` 等（需要 3 级流水）
- 分支跳转：`beq`, `jal` 等

## 项目结构

```
VLIW_PACK_Analyzer/
├── analyzer.py           # 主分析器
├── parser.py            # 反汇编文件解析器
├── instruction.py       # 指令类定义
├── dependency.py        # 依赖关系分析
├── packer.py            # VLIW 重打包算法
├── statistics.py        # 统计与报告生成
├── config.py            # 配置文件（指令类型定义）
├── main.py              # 命令行入口
├── tests/               # 单元测试
│   ├── test_parser.py
│   ├── test_dependency.py
│   ├── test_packer.py
│   └── test_statistics.py
├── PLAN.md              # 详细实现计划
├── GITHUB_ISSUE.md      # GitHub Issue 格式文档
└── README.md            # 本文件
```

## 算法说明

### 一层依赖重打包算法

```
输入：原始 VLIW 包列表
输出：优化后的 VLIW 包列表

1. 提取所有有效指令（非填充）到指令流
2. 构建依赖图：
   - 标记每条指令的依赖关系
   - 识别单周期 ALU 指令
3. 贪心打包：
   初始化：当前包 = []
   遍历指令流：
     如果当前包未满（<8条）：
       检查当前指令是否可以加入：
         - 无依赖 → 可加入
         - 依赖已在前面的包 → 可加入
         - 依赖在当前包且形成一层依赖 → 可加入
         - 否则 → 不可加入
     如果可加入：加入当前包
     否则：保存当前包，创建新包
4. 返回优化后的包列表
```

**保证正确性**：
- 不跨越分支边界重排指令
- 保持 Load/Store 的相对顺序
- 严格遵守数据依赖关系

## 开发计划

详细的实现计划请参考 [PLAN.md](PLAN.md)。

### 实现阶段

1. **Phase 1**：基础框架（2-3 天）
2. **Phase 2**：统计分析（2 天）
3. **Phase 3**：依赖分析（3 天）
4. **Phase 4**：重打包算法（3-4 天）
5. **Phase 5**：完善与测试（2 天）

**总计**：约 12-14 天

## 测试

```bash
# 运行所有测试
python -m pytest tests/

# 运行特定测试
python -m pytest tests/test_parser.py

# 查看覆盖率
python -m pytest --cov=. tests/
```

## 贡献

欢迎贡献代码、报告问题或提出建议！

## 许可证

本项目遵循 Zircon-VLIW 项目的许可证。

## 参考资料

- [Zircon-VLIW 架构文档](../docs/arch.md)
- [RISC-V 指令集手册](https://riscv.org/specifications/)
- [VLIW 架构介绍](https://en.wikipedia.org/wiki/Very_long_instruction_word)

## 联系方式

如有问题，请在 GitHub 上提交 Issue。

---

**版本**：0.1.0  
**最后更新**：2026-01-04

