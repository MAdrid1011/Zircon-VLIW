# [plan][feature] 开发 VLIW 反汇编分析工具

## Problem Statement

当前 Zircon-VLIW 项目使用 VLIW 编译器生成的反汇编代码（如 `VLIW_PACK/FFT/build/FFT-riscv32.txt`）包含大量填充指令（`nop` 和 `feq.s zero, ft0, ft0`），且 VLIW 包的利用率不高。我们需要一个分析工具来：

1. **评估指令打包优化潜力**：如果允许一层依赖关系（producer 和 consumer 都是单周期算术指令），VLIW 包数量可以减少多少？
2. **量化填充指令开销**：如果删除包前和包后的填充指令，程序大小可以缩小多少？

这些分析结果将指导编译器优化和架构改进决策。

### 背景信息

- **VLIW 包结构**：每个包包含 8 条指令
- **填充指令**：
  - `nop` (0x00000013)
  - `feq.s zero, ft0, ft0` (0xa0002053)
- **单周期算术指令**：基本 ALU 操作（加减、逻辑运算、移位等），不包括乘除法
- **一层依赖**：指令 B 依赖指令 A 的结果，且 A 和 B 都是单周期指令

## Proposed Solution

开发一个独立的 Python 工具 `VLIW_PACK_Analyzer`，提供以下核心功能：

### 1. 架构设计

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
├── requirements.txt     # Python 依赖
├── PLAN.md              # 详细实现计划
└── README.md            # 使用说明
```

### 2. 核心功能模块

#### 2.1 反汇编解析器 (`parser.py`)

- 解析 objdump 格式的反汇编文件
- 识别 VLIW 包边界（每 8 条指令）
- 提取指令信息：地址、编码、助记符、操作数
- 识别填充指令

#### 2.2 指令表示 (`instruction.py`)

```python
class Instruction:
    - address: int          # 指令地址
    - mnemonic: str         # 指令助记符
    - rd, rs1, rs2, rs3     # 寄存器
    - is_nop: bool          # 是否为填充指令
    - is_single_cycle: bool # 是否为单周期指令
    - inst_type: str        # 指令类型

class VLIWPackage:
    - instructions: List[Instruction]  # 8条指令
    - valid_count: int                 # 有效指令数
```

#### 2.3 依赖分析器 (`dependency.py`)

- 检测 RAW（Read After Write）依赖
- 识别单周期算术指令
- 判断是否可以形成一层依赖
- 构建指令依赖图

**一层依赖判定规则**：
```python
def can_form_one_level_dependency(producer, consumer):
    return (
        producer.is_single_cycle_alu and
        consumer.is_single_cycle_alu and
        consumer depends on producer.rd
    )
```

#### 2.4 VLIW 重打包器 (`packer.py`)

**算法**：贪心打包，允许一层依赖

```
1. 提取所有有效指令（非填充）
2. 按顺序遍历指令：
   a. 如果当前包未满（<8条）且满足以下条件之一：
      - 指令无依赖
      - 依赖已在前面的包中
      - 依赖在当前包中且形成一层依赖
   b. 则加入当前包
   c. 否则创建新包
3. 统计优化后的包数量
```

**约束**：
- 保持程序语义正确性
- 不跨越分支边界重排
- 保持 Load/Store 相对顺序

#### 2.5 统计分析器 (`statistics.py`)

**原始包统计**：
- 总包数、总指令数
- 有效指令数、填充指令数
- 平均每包有效指令数

**填充指令分析**：
- 包前填充：包开头的连续填充指令
- 包后填充：包末尾的连续填充指令
- 包中填充：夹在有效指令之间的填充
- 可删除数量：包前 + 包后
- 程序大小缩减：可删除数 × 4 字节

**重打包对比**：
- 优化后包数量
- 包数量减少比例
- 指令密度提升
- 成功合并的依赖对数量

### 3. 命令行接口

```bash
# 基本用法
python main.py VLIW_PACK/FFT/build/FFT-riscv32.txt

# 指定输出文件
python main.py FFT-riscv32.txt --output report.txt

# 批量分析
python main.py VLIW_PACK/functest/build/*.txt --batch

# 详细输出
python main.py FFT-riscv32.txt --verbose
```

### 4. 输出报告示例

```
=== VLIW 反汇编分析报告 ===

文件：FFT-riscv32.txt

--- 原始包统计 ---
总包数：274
总指令数：2192
有效指令数：1245 (56.8%)
填充指令数：947 (43.2%)
平均每包有效指令：4.54

--- 填充指令分析 ---
包前填充：156 条
包后填充：189 条
可删除填充：345 条
程序大小缩减：1380 字节 (15.7%)

--- 一层依赖重打包分析 ---
优化后包数：198
包数量减少：76 (27.7%)
平均每包有效指令：6.29
指令密度提升：38.5%
成功合并依赖对：134 对

=== 分析完成 ===
```

### 5. 实现步骤

**Phase 1：基础框架** (2-3 天)
- [ ] 创建项目结构
- [ ] 实现 `Instruction` 和 `VLIWPackage` 类
- [ ] 实现反汇编解析器
- [ ] 单元测试：解析 FFT-riscv32.txt

**Phase 2：统计分析** (2 天)
- [ ] 实现原始包统计
- [ ] 实现填充指令识别和分析
- [ ] 生成基础报告
- [ ] 验证统计准确性

**Phase 3：依赖分析** (3 天)
- [ ] 实现寄存器提取（支持别名：a0-a7, s0-s11 等）
- [ ] 实现 RAW 依赖检测
- [ ] 实现单周期指令识别
- [ ] 构建依赖图
- [ ] 单元测试：依赖检测

**Phase 4：重打包算法** (3-4 天)
- [ ] 实现一层依赖判定
- [ ] 实现贪心打包算法
- [ ] 验证程序语义正确性
- [ ] 处理边界情况（分支、Load/Store）
- [ ] 集成测试

**Phase 5：完善与测试** (2 天)
- [ ] 添加命令行接口
- [ ] 测试多个反汇编文件
- [ ] 性能优化
- [ ] 编写 README 和使用文档

**总计**：约 12-14 天

### 6. 技术要点

**指令分类**（基于 Zircon-VLIW 架构）：

```python
# 单周期 ALU 指令
SINGLE_CYCLE_ALU = {
    'add', 'addi', 'sub', 'and', 'andi', 'or', 'ori', 
    'xor', 'xori', 'sll', 'slli', 'srl', 'srli', 
    'sra', 'srai', 'slt', 'slti', 'sltu', 'sltiu',
    'lui', 'auipc'
}

# 多周期指令（排除）
MULTI_CYCLE = {
    'mul', 'div', 'rem',  # 乘除法
    'lw', 'sw', 'flw', 'fsw',  # Load/Store
    'fadd.s', 'fmul.s', 'fdiv.s'  # 浮点运算
}
```

**寄存器别名处理**：
- 整数寄存器：x0-x31, zero, ra, sp, gp, tp, a0-a7, s0-s11, t0-t6
- 浮点寄存器：f0-f31, ft0-ft11, fa0-fa7, fs0-fs11

**依赖检测注意事项**：
- 区分整数和浮点寄存器
- 处理 x0/zero 寄存器（写入无效）
- 识别隐式依赖（如 `ret` 依赖 `ra`）

### 7. 验证方法

1. **解析验证**：手工检查部分解析结果
2. **统计验证**：对比手工统计的包数量
3. **依赖验证**：可视化部分依赖关系
4. **重打包验证**：确保优化后指令顺序合法

### 8. 扩展性

未来可扩展功能：
- 支持多层依赖优化
- 生成优化后的汇编代码
- 可视化依赖图（GraphViz）
- 性能模拟（估算周期数）
- 支持其他 VLIW 架构

## Test Strategy

### 单元测试

1. **指令解析测试** (`test_parser.py`)
   - 测试各类指令格式解析
   - 测试填充指令识别
   - 测试边界情况（空行、注释）

2. **依赖检测测试** (`test_dependency.py`)
   ```python
   # 测试用例
   inst1 = Instruction("add a0, a1, a2")  # a0 = a1 + a2
   inst2 = Instruction("addi a3, a0, 1")  # a3 = a0 + 1
   assert analyze_dependency(inst1, inst2) == True
   assert can_form_one_level_dependency(inst1, inst2) == True
   ```

3. **填充指令测试** (`test_statistics.py`)
   - 测试包前/包后填充识别
   - 测试统计准确性

### 集成测试

1. **完整流程测试**
   - 使用 `FFT-riscv32.txt` 运行完整分析
   - 验证报告各项数据合理性

2. **多文件测试**
   - 批量测试 `functest/build/*.txt`
   - 确保工具稳定性

3. **边界情况测试**
   - 空文件
   - 只有填充指令的包
   - 无依赖的指令序列
   - 复杂依赖链

### 验证测试

1. **手工验证**
   - 随机选择 5-10 个 VLIW 包
   - 手工验证重打包结果正确性
   - 确认依赖关系保持

2. **语义验证**
   - 确保重打包后程序语义不变
   - 检查分支边界未被跨越
   - 检查 Load/Store 顺序未改变

### 性能测试

- 测试大文件处理速度（如 FFT 2195 行）
- 目标：<5 秒完成分析

### 测试覆盖率

- 目标代码覆盖率：>80%
- 关键模块（依赖分析、重打包）：>90%

## Acceptance Criteria

- [ ] 能够正确解析 RISC-V 反汇编文件
- [ ] 准确识别 VLIW 包边界（每 8 条指令）
- [ ] 正确统计填充指令数量和位置
- [ ] 准确计算程序大小缩减比例
- [ ] 正确检测指令间的 RAW 依赖
- [ ] 准确识别单周期算术指令
- [ ] 重打包算法保持程序语义正确性
- [ ] 生成清晰易读的分析报告
- [ ] 通过所有单元测试和集成测试
- [ ] 提供完整的使用文档

## Dependencies

- Python >= 3.8
- 无外部依赖（仅使用标准库）

## Estimated Effort

- 开发时间：12-14 天
- 测试时间：2-3 天
- 文档编写：1 天
- **总计**：约 15-18 天

## References

- Zircon-VLIW 架构文档：`docs/arch.md`
- 示例反汇编文件：`VLIW_PACK/FFT/build/FFT-riscv32.txt`
- RISC-V 指令集手册：https://riscv.org/specifications/

---

**标签**：`[plan][feature]`, `tools`, `analysis`, `optimization`, `VLIW`

