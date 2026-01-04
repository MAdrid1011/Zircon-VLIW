# VLIW 反汇编分析工具实现计划

## 项目概述

开发一个 Python 工具，用于分析 VLIW 反汇编文件（如 `FFT-riscv32.txt`），评估指令打包优化潜力。

## 目标

1. **依赖分析与重打包**：分析允许一层依赖时的 VLIW 包数量优化
   - 一层依赖：目标指令和被依赖指令都必须是 1 周期算术指令
   - 排除：乘除法指令
   - 对比：原始包数量 vs 优化后包数量

2. **填充指令分析**：计算删除包前/包后填充指令后的程序大小缩减
   - 识别填充指令：`nop` 和 `feq.s zero, ft0, ft0`
   - 统计：原始大小 vs 优化后大小

## 技术设计

### 1. 项目结构

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
└── README.md            # 使用说明
```

### 2. 核心模块设计

#### 2.1 `instruction.py` - 指令表示

```python
class Instruction:
    """表示单条 RISC-V 指令"""
    - address: int          # 指令地址
    - hex_code: str         # 十六进制编码
    - mnemonic: str         # 指令助记符
    - operands: List[str]   # 操作数列表
    - rd: Optional[str]     # 目标寄存器
    - rs1: Optional[str]    # 源寄存器1
    - rs2: Optional[str]    # 源寄存器2
    - rs3: Optional[str]    # 源寄存器3（浮点）
    - is_nop: bool          # 是否为填充指令
    - is_single_cycle: bool # 是否为单周期指令
    - inst_type: str        # 指令类型（ALU/Load/Store/Branch等）

class VLIWPackage:
    """表示一个 VLIW 包（8条指令）"""
    - start_address: int
    - instructions: List[Instruction]  # 最多8条
    - valid_count: int                 # 有效指令数（非nop/feq.s zero）
```

#### 2.2 `parser.py` - 反汇编解析

```python
class DisassemblyParser:
    """解析反汇编文件"""
    
    def parse_file(filepath: str) -> List[VLIWPackage]:
        """解析整个文件，返回 VLIW 包列表"""
        
    def parse_instruction(line: str) -> Instruction:
        """解析单行指令"""
        
    def identify_packages(instructions: List[Instruction]) -> List[VLIWPackage]:
        """识别 VLIW 包边界（每8条指令一包）"""
        
    def is_padding_instruction(inst: Instruction) -> bool:
        """判断是否为填充指令"""
        # nop: 00000013
        # feq.s zero, ft0, ft0: a0002053
```

#### 2.3 `config.py` - 指令分类配置

```python
# 单周期算术指令（不包括乘除法）
SINGLE_CYCLE_ALU = {
    'add', 'addi', 'sub', 'and', 'andi', 'or', 'ori', 'xor', 'xori',
    'sll', 'slli', 'srl', 'srli', 'sra', 'srai',
    'slt', 'slti', 'sltu', 'sltiu',
    'lui', 'auipc', 'mv', 'li'
}

# 多周期指令（不能参与一层依赖优化）
MULTI_CYCLE_INST = {
    'mul', 'mulh', 'mulhsu', 'mulhu',
    'div', 'divu', 'rem', 'remu',
    'lw', 'lh', 'lb', 'lhu', 'lbu', 'flw',
    'sw', 'sh', 'sb', 'fsw',
    'fadd.s', 'fsub.s', 'fmul.s', 'fdiv.s', 'fsqrt.s',
    'fmadd.s', 'fmsub.s', 'fnmadd.s', 'fnmsub.s'
}

# 分支/跳转指令
BRANCH_JUMP_INST = {
    'beq', 'bne', 'blt', 'bge', 'bltu', 'bgeu',
    'jal', 'jalr', 'ret'
}

# 填充指令
PADDING_INST = {
    'nop': '00000013',
    'feq.s zero': 'a0002053'
}
```

#### 2.4 `dependency.py` - 依赖分析

```python
class DependencyAnalyzer:
    """分析指令间的数据依赖关系"""
    
    def analyze_raw_dependency(inst1: Instruction, inst2: Instruction) -> bool:
        """检查 inst2 是否依赖 inst1（RAW）"""
        # inst1.rd 在 inst2.rs1/rs2/rs3 中
        
    def can_form_one_level_dependency(producer: Instruction, consumer: Instruction) -> bool:
        """检查是否可以形成一层依赖"""
        # 两者都必须是单周期算术指令
        # consumer 依赖 producer
        
    def build_dependency_graph(instructions: List[Instruction]) -> Dict:
        """构建依赖图"""
        # 返回每条指令的依赖关系
```

#### 2.5 `packer.py` - VLIW 重打包算法

```python
class VLIWPacker:
    """VLIW 指令重打包优化"""
    
    def repack_with_one_level_dependency(
        packages: List[VLIWPackage],
        dep_graph: Dict
    ) -> List[VLIWPackage]:
        """允许一层依赖的重打包"""
        
        算法流程：
        1. 遍历所有包，提取有效指令
        2. 按照依赖关系和执行顺序重新组织
        3. 在同一个包内允许：
           - 独立指令（无依赖）
           - 一层依赖对（producer -> consumer）
           - 限制：producer 和 consumer 都是单周期 ALU 指令
        4. 贪心打包：尽量填满每个包（最多8条）
        5. 保持程序语义正确性
        
    def calculate_package_reduction(
        original: List[VLIWPackage],
        optimized: List[VLIWPackage]
    ) -> Dict:
        """计算包数量减少统计"""
```

#### 2.6 `statistics.py` - 统计分析

```python
class StatisticsCollector:
    """收集和生成统计报告"""
    
    def analyze_original_packages(packages: List[VLIWPackage]) -> Dict:
        """分析原始包统计"""
        - 总包数
        - 总指令数
        - 有效指令数（非填充）
        - 填充指令数
        - 平均每包有效指令数
        
    def analyze_padding_instructions(packages: List[VLIWPackage]) -> Dict:
        """分析填充指令统计"""
        - 包前填充（连续的填充指令在包开头）
        - 包后填充（连续的填充指令在包末尾）
        - 包中填充（夹在有效指令之间）
        - 可删除的填充指令数
        - 程序大小缩减比例
        
    def compare_packing_results(
        original: List[VLIWPackage],
        optimized: List[VLIWPackage]
    ) -> Dict:
        """对比打包结果"""
        - 包数量变化
        - 指令密度提升
        - 优化比例
        
    def generate_report(stats: Dict) -> str:
        """生成可读的分析报告"""
```

#### 2.7 `analyzer.py` - 主分析器

```python
class VLIWAnalyzer:
    """主分析器，协调各模块"""
    
    def __init__(self, filepath: str):
        self.filepath = filepath
        self.parser = DisassemblyParser()
        self.dep_analyzer = DependencyAnalyzer()
        self.packer = VLIWPacker()
        self.stats = StatisticsCollector()
        
    def run_full_analysis(self) -> Dict:
        """运行完整分析流程"""
        1. 解析反汇编文件
        2. 识别 VLIW 包
        3. 分析原始包统计
        4. 构建依赖图
        5. 重打包（允许一层依赖）
        6. 分析填充指令
        7. 生成对比报告
        8. 返回所有统计数据
```

#### 2.8 `main.py` - 命令行接口

```python
def main():
    """命令行入口"""
    
    参数：
    - input_file: 反汇编文件路径
    - --output: 输出报告路径（可选）
    - --format: 报告格式（text/json/html）
    - --verbose: 详细输出
    
    示例：
    python main.py FFT-riscv32.txt --output report.txt --format text
```

### 3. 算法细节

#### 3.1 一层依赖重打包算法

```
输入：原始 VLIW 包列表
输出：优化后的 VLIW 包列表

1. 提取所有有效指令（非填充）到指令流
2. 为每条指令标记：
   - 依赖的指令集合
   - 是否为单周期 ALU 指令
3. 初始化：当前包 = []，已打包指令 = []
4. 遍历指令流：
   a. 如果当前包未满（<8条）：
      - 检查当前指令是否可以加入：
        * 无依赖 → 可加入
        * 依赖已在当前包 → 检查是否为一层依赖
          - 是单周期 ALU 对 → 可加入
          - 否 → 不可加入
        * 依赖不在当前包 → 检查依赖是否已打包
          - 是 → 可加入
          - 否 → 不可加入
   b. 如果可加入：加入当前包
   c. 如果不可加入或包已满：
      - 保存当前包
      - 创建新包
      - 重试当前指令
5. 返回优化后的包列表
```

#### 3.2 填充指令识别算法

```
对于每个 VLIW 包：
1. 扫描包开头：
   - 统计连续的填充指令数量 → 包前填充
2. 扫描包末尾：
   - 统计连续的填充指令数量 → 包后填充
3. 扫描包中间：
   - 统计夹在有效指令之间的填充指令 → 包中填充
4. 计算可删除的填充：
   - 包前 + 包后（包中填充可能用于对齐，需保留）
5. 计算大小缩减：
   - 可删除指令数 × 4 字节
```

### 4. 输出报告格式

```
=== VLIW 反汇编分析报告 ===

文件：FFT-riscv32.txt
分析时间：2026-01-04 12:00:00

--- 原始包统计 ---
总包数：274
总指令数：2192 (274 × 8)
有效指令数：1245
填充指令数：947
  - nop: 623
  - feq.s zero: 324
平均每包有效指令：4.54

--- 填充指令分析 ---
包前填充：156 条
包后填充：189 条
包中填充：602 条
可删除填充：345 条 (包前 + 包后)
程序大小缩减：1380 字节 (15.7%)

--- 一层依赖重打包分析 ---
优化后包数：198
包数量减少：76 (27.7%)
有效指令数：1245 (不变)
平均每包有效指令：6.29
指令密度提升：38.5%

单周期 ALU 指令：892
可形成一层依赖对：156 对
成功合并到同一包：134 对

--- 详细统计 ---
指令类型分布：
  ALU 指令：892 (71.6%)
  Load/Store：234 (18.8%)
  分支跳转：67 (5.4%)
  乘除法：32 (2.6%)
  浮点运算：20 (1.6%)

依赖关系统计：
  无依赖指令：567
  有依赖指令：678
    - 一层依赖：312
    - 多层依赖：366

=== 分析完成 ===
```

### 5. 实现步骤

1. **Phase 1：基础框架**
   - 创建项目结构
   - 实现 `Instruction` 和 `VLIWPackage` 类
   - 实现基本的反汇编解析器

2. **Phase 2：统计分析**
   - 实现原始包统计
   - 实现填充指令识别和统计
   - 生成基础报告

3. **Phase 3：依赖分析**
   - 实现寄存器提取
   - 实现依赖关系检测
   - 构建依赖图

4. **Phase 4：重打包算法**
   - 实现一层依赖检测
   - 实现贪心打包算法
   - 验证正确性

5. **Phase 5：完善与测试**
   - 添加命令行接口
   - 测试多个反汇编文件
   - 优化性能
   - 完善文档

### 6. 技术挑战与解决方案

**挑战1：正确识别寄存器依赖**
- 解决：使用正则表达式精确提取寄存器名
- 注意：区分整数寄存器（x0-x31/a0-a7/s0-s11等）和浮点寄存器（f0-f31/ft0-ft11等）

**挑战2：保持程序语义正确性**
- 解决：重打包时严格遵守依赖关系
- 不允许：跨越分支边界重排指令
- 不允许：改变 Load/Store 的相对顺序

**挑战3：识别单周期指令**
- 解决：维护完整的指令分类表
- 参考：Zircon-VLIW 架构文档中的流水线定义

**挑战4：处理伪指令**
- 解决：将伪指令映射到实际指令
- 例如：`mv` → `addi`，`li` → `addi` 或 `lui`

### 7. 依赖项

```txt
# requirements.txt
# 无外部依赖，仅使用 Python 标准库
# Python >= 3.8
```

### 8. 测试计划

1. **单元测试**
   - 指令解析测试
   - 依赖检测测试
   - 填充指令识别测试

2. **集成测试**
   - 使用 FFT-riscv32.txt 完整测试
   - 使用其他反汇编文件测试（functest/）

3. **验证测试**
   - 手工验证部分重打包结果
   - 确保指令顺序合法性

### 9. 扩展性考虑

未来可扩展功能：
- 支持多层依赖优化
- 支持跨基本块优化
- 可视化依赖图
- 生成优化后的汇编代码
- 性能模拟（周期数估算）

---

## 实现优先级

**P0（核心功能）**
- 反汇编解析
- 原始包统计
- 填充指令分析

**P1（主要功能）**
- 依赖关系分析
- 一层依赖重打包
- 对比报告生成

**P2（完善功能）**
- 命令行接口
- 多文件批处理
- 详细日志输出

**P3（可选功能）**
- HTML 报告
- 可视化图表
- 性能优化

