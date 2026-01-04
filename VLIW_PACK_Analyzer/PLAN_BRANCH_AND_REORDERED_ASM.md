# 实现计划：Branch 指令优化 & 重排反汇编输出

## 问题陈述

当前 VLIW_PACK_Analyzer 存在两个问题：

1. **Branch 类指令未参与一层依赖计算**：目前 `beq`, `bne`, `jal`, `jalr` 等分支/跳转指令被归类为 `BRANCH` 类型，不参与单周期 ALU 的一层依赖优化。实际上在 Zircon-VLIW 架构中，这些指令也是单周期完成的，应该可以与前面的 ALU 指令形成一层依赖。

2. **缺少重排后反汇编输出**：当前工具只输出统计报告，没有输出重排后的反汇编文件，不便于验证和调试。

## 提议方案

### 功能 1：将 Branch 类指令视为单周期 ALU 指令

#### 修改文件：`config.py`

新增一个配置项，标识哪些指令可以参与一层依赖：

```python
# 可参与一层依赖的指令（单周期完成，EX2 阶段可前递）
# 包括：单周期 ALU + 分支/跳转指令
ONE_LEVEL_DEPENDENCY_ELIGIBLE = SINGLE_CYCLE_ALU | BRANCH_JUMP_INST
```

#### 修改文件：`instruction.py`

在 `Instruction` 类中添加新属性：

```python
# 在 __init__ 中新增
self.can_one_level_dep = self._check_can_one_level_dependency()

def _check_can_one_level_dependency(self) -> bool:
    """检查是否可以参与一层依赖"""
    from config import ONE_LEVEL_DEPENDENCY_ELIGIBLE
    return self.mnemonic in ONE_LEVEL_DEPENDENCY_ELIGIBLE
```

#### 修改文件：`dependency.py`

修改 `can_form_one_level_dependency` 方法：

```python
def can_form_one_level_dependency(self, producer: Instruction, consumer: Instruction) -> bool:
    """
    检查是否可以形成一层依赖
    
    修改后的条件：
    1. consumer 依赖 producer
    2. producer 是单周期 ALU 指令
    3. consumer 可以参与一层依赖（单周期 ALU 或 Branch 指令）
    """
    # producer 必须是单周期 ALU（生产值）
    if not producer.is_single_cycle:
        return False
    
    # consumer 必须可以参与一层依赖
    if not consumer.can_one_level_dep:
        return False
    
    # consumer 必须依赖 producer
    if not self.analyze_raw_dependency(producer, consumer):
        return False
    
    return True
```

### 功能 2：输出重排后的反汇编

#### 新增文件：`exporter.py`

创建新的导出器模块：

```python
"""
反汇编导出器：输出重排后的反汇编文件
"""

from typing import List
from instruction import Instruction, VLIWPackage


class DisassemblyExporter:
    """导出重排后的反汇编"""
    
    def export_reordered_asm(
        self,
        original_packages: List[VLIWPackage],
        optimized_packages: List[VLIWPackage],
        output_path: str
    ):
        """
        导出重排后的反汇编文件
        
        算法：
        1. 收集所有原始地址，按顺序排列
        2. 按优化后的包顺序，将指令填入对应地址槽
        3. 生成反汇编格式输出
        
        Args:
            original_packages: 原始 VLIW 包列表（用于获取地址）
            optimized_packages: 优化后的 VLIW 包列表
            output_path: 输出文件路径
        """
        # 收集所有原始地址
        original_addresses = []
        for pkg in original_packages:
            for inst in pkg.instructions:
                original_addresses.append(inst.address)
        
        # 收集优化后的指令（包括填充到满 8 条）
        reordered_instructions = []
        for pkg in optimized_packages:
            # 添加包中的有效指令
            for inst in pkg.instructions:
                reordered_instructions.append(inst)
            # 填充到 8 条（使用 NOP）
            while len(reordered_instructions) % 8 != 0:
                reordered_instructions.append(None)  # 标记为 NOP
        
        # 生成输出
        lines = []
        lines.append("# VLIW 重排后反汇编")
        lines.append("# 注意：PC 地址保持原始顺序，指令已根据一层依赖优化重排")
        lines.append("")
        
        for i, addr in enumerate(original_addresses):
            if i < len(reordered_instructions) and reordered_instructions[i] is not None:
                inst = reordered_instructions[i]
                line = f"{addr:08x}: {inst.hex_code}     \t{inst.mnemonic}\t{inst.operands}"
            else:
                # NOP 填充
                line = f"{addr:08x}: 00000013     \tnop"
            
            # 每 8 条加分隔（包边界）
            if i % 8 == 0 and i > 0:
                lines.append("")
            
            lines.append(line)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write('\n'.join(lines))
```

#### 修改文件：`analyzer.py`

在 `VLIWAnalyzer` 类中添加导出功能：

```python
from exporter import DisassemblyExporter

# 在 __init__ 中添加
self.exporter = DisassemblyExporter()

# 新增方法
def export_reordered_asm(self, output_path: str):
    """
    导出重排后的反汇编文件
    
    Args:
        output_path: 输出文件路径
    """
    if not self.optimized_packages:
        print("错误：尚未运行分析，请先调用 run_full_analysis()")
        return
    
    self.exporter.export_reordered_asm(
        self.original_packages,
        self.optimized_packages,
        output_path
    )
    print(f"重排后反汇编已保存到: {output_path}")
```

#### 修改文件：`main.py`

添加命令行选项：

```python
parser.add_argument(
    '--export-asm', '-e',
    help='导出重排后的反汇编文件路径',
    default=None
)

# 在 main() 中添加
if args.export_asm:
    analyzer.export_reordered_asm(args.export_asm)
```

## 测试策略

1. **单元测试**：验证 Branch 指令的 `can_one_level_dep` 属性正确设置
2. **集成测试**：使用现有的 FFT-riscv32.txt 文件，对比修改前后的优化包数
3. **输出验证**：检查重排后的反汇编文件格式正确，地址对齐正确

## 文件修改清单

| 文件 | 修改类型 | 描述 |
|------|----------|------|
| `config.py` | 修改 | 新增 `ONE_LEVEL_DEPENDENCY_ELIGIBLE` 配置 |
| `instruction.py` | 修改 | 添加 `can_one_level_dep` 属性 |
| `dependency.py` | 修改 | 更新 `can_form_one_level_dependency` 逻辑 |
| `exporter.py` | 新增 | 反汇编导出器模块 |
| `analyzer.py` | 修改 | 集成导出功能 |
| `main.py` | 修改 | 添加 `--export-asm` 命令行选项 |

## 预期效果

修改后，运行：
```bash
python main.py ../VLIW_PACK/FFT/build/FFT-riscv32.txt -e reordered.txt
```

将会：
1. 在优化包数计算中考虑 Branch 指令的一层依赖
2. 输出 `reordered.txt` 文件，包含重排后的反汇编

