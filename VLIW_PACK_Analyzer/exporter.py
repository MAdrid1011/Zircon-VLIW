"""
反汇编导出器：输出重排后的反汇编文件
"""

from typing import List
from instruction import Instruction, VLIWPackage
from config import VLIW_PACKAGE_SIZE


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
        3. 生成反汇编格式输出（PC 保持原有值）
        
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
        
        # 收集优化后的指令（按包顺序，每包填充到 8 条）
        reordered_instructions = []
        for pkg in optimized_packages:
            # 添加包中的有效指令
            pkg_instructions = list(pkg.instructions)
            reordered_instructions.extend(pkg_instructions)
            # 填充到 8 条（使用 None 标记 NOP）
            padding_count = VLIW_PACKAGE_SIZE - len(pkg_instructions)
            reordered_instructions.extend([None] * padding_count)
        
        # 生成输出
        lines = []
        lines.append("# VLIW 重排后反汇编")
        lines.append("# 注意：PC 地址保持原始顺序，指令已根据一层依赖优化重排")
        lines.append(f"# 原始包数：{len(original_packages)}，优化后包数：{len(optimized_packages)}")
        lines.append("")
        
        for i, addr in enumerate(original_addresses):
            # 每 8 条加包边界注释
            if i % VLIW_PACKAGE_SIZE == 0:
                pkg_idx = i // VLIW_PACKAGE_SIZE
                if pkg_idx < len(optimized_packages):
                    valid_count = optimized_packages[pkg_idx].valid_count
                    lines.append(f"\n# === Package {pkg_idx} (有效指令: {valid_count}) ===")
                else:
                    lines.append(f"\n# === Package {pkg_idx} (已优化掉) ===")
            
            if i < len(reordered_instructions) and reordered_instructions[i] is not None:
                inst = reordered_instructions[i]
                # 格式化输出，与 objdump 格式类似
                line = f"{addr:08x}: {inst.hex_code}     \t{inst.mnemonic}\t{inst.operands}"
            else:
                # NOP 填充
                line = f"{addr:08x}: 00000013     \tnop"
            
            lines.append(line)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write('\n'.join(lines))
        
        return len(optimized_packages)
    
    def export_compact_asm(
        self,
        optimized_packages: List[VLIWPackage],
        output_path: str,
        base_address: int = 0x80000000
    ):
        """
        导出紧凑格式的重排反汇编（不含 NOP 填充）
        
        Args:
            optimized_packages: 优化后的 VLIW 包列表
            output_path: 输出文件路径
            base_address: 起始地址
        """
        lines = []
        lines.append("# VLIW 紧凑格式反汇编（仅有效指令）")
        lines.append(f"# 优化后包数：{len(optimized_packages)}")
        lines.append("")
        
        current_addr = base_address
        
        for pkg_idx, pkg in enumerate(optimized_packages):
            lines.append(f"\n# === Package {pkg_idx} (有效指令: {pkg.valid_count}) ===")
            
            for inst in pkg.instructions:
                if not inst.is_nop:
                    line = f"{current_addr:08x}: {inst.hex_code}     \t{inst.mnemonic}\t{inst.operands}"
                    lines.append(line)
                current_addr += 4
            
            # 补齐到 8 条的地址空间
            padding_count = VLIW_PACKAGE_SIZE - len(pkg.instructions)
            current_addr += padding_count * 4
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write('\n'.join(lines))

