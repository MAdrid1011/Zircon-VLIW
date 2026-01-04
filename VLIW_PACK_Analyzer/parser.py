"""
反汇编文件解析器：解析 objdump 格式的 RISC-V 反汇编文件
"""

import re
from typing import List
from instruction import Instruction, VLIWPackage
from config import VLIW_PACKAGE_SIZE


class DisassemblyParser:
    """解析反汇编文件"""
    
    def __init__(self):
        # 指令行正则表达式
        # 格式：80000000: 00000413     	li	s0, 0x0
        # 或：  80000000: 00000413     	li	s0, 0
        self.inst_pattern = re.compile(
            r'^([0-9a-f]+):\s+([0-9a-f]+)\s+(.+)$',
            re.IGNORECASE
        )
    
    def parse_file(self, filepath: str) -> List[VLIWPackage]:
        """
        解析整个文件，返回 VLIW 包列表
        
        Args:
            filepath: 反汇编文件路径
            
        Returns:
            VLIW 包列表
        """
        instructions = []
        
        with open(filepath, 'r', encoding='utf-8') as f:
            for line in f:
                inst = self.parse_instruction(line)
                if inst:
                    instructions.append(inst)
        
        # 识别 VLIW 包边界
        packages = self.identify_packages(instructions)
        return packages
    
    def parse_instruction(self, line: str) -> Instruction:
        """
        解析单行指令
        
        Args:
            line: 反汇编文件中的一行
            
        Returns:
            Instruction 对象，如果不是指令行则返回 None
        """
        line = line.strip()
        
        # 跳过空行和注释
        if not line or line.startswith('#'):
            return None
        
        # 跳过段标记和函数标记
        if line.startswith('Disassembly') or line.endswith(':') and not self.inst_pattern.match(line):
            return None
        
        # 匹配指令行
        match = self.inst_pattern.match(line)
        if not match:
            return None
        
        address_str = match.group(1)
        hex_code = match.group(2)
        rest = match.group(3).strip()
        
        # 解析地址
        address = int(address_str, 16)
        
        # 分割助记符和操作数
        # 格式可能是：li	s0, 0x0 或 add	a0, a1, a2
        parts = rest.split(None, 1)  # 按空白字符分割，最多分割一次
        
        if len(parts) == 0:
            return None
        
        mnemonic = parts[0]
        operands = parts[1] if len(parts) > 1 else ''
        
        return Instruction(address, hex_code, mnemonic, operands)
    
    def identify_packages(self, instructions: List[Instruction]) -> List[VLIWPackage]:
        """
        识别 VLIW 包边界（每 8 条指令一包）
        
        Args:
            instructions: 指令列表
            
        Returns:
            VLIW 包列表
        """
        packages = []
        current_package = None
        
        for i, inst in enumerate(instructions):
            # 每 8 条指令创建一个新包
            if i % VLIW_PACKAGE_SIZE == 0:
                if current_package:
                    packages.append(current_package)
                current_package = VLIWPackage(inst.address)
            
            current_package.add_instruction(inst)
        
        # 添加最后一个包
        if current_package and current_package.instructions:
            packages.append(current_package)
        
        return packages
    
    def is_padding_instruction(self, inst: Instruction) -> bool:
        """
        判断是否为填充指令
        
        Args:
            inst: 指令对象
            
        Returns:
            是否为填充指令
        """
        return inst.is_nop

