"""
指令类定义：表示单条 RISC-V 指令和 VLIW 包
"""

from typing import List, Optional
from config import SINGLE_CYCLE_ALU, MULTI_CYCLE_INST, BRANCH_JUMP_INST, PADDING_INST
from config import INT_REG_ALIAS, FLOAT_REG_ALIAS


class Instruction:
    """表示单条 RISC-V 指令"""
    
    def __init__(self, address: int, hex_code: str, mnemonic: str, operands: str):
        self.address = address
        self.hex_code = hex_code.strip()
        self.mnemonic = mnemonic.strip()
        self.operands = operands.strip()
        
        # 寄存器字段
        self.rd: Optional[str] = None
        self.rs1: Optional[str] = None
        self.rs2: Optional[str] = None
        self.rs3: Optional[str] = None
        
        # 指令属性
        self.is_nop = self._check_is_nop()
        self.is_single_cycle = self._check_is_single_cycle()
        self.inst_type = self._determine_type()
        
        # 解析操作数，提取寄存器
        self._parse_operands()
    
    def _check_is_nop(self) -> bool:
        """检查是否为填充指令"""
        # 检查 nop
        if self.hex_code == PADDING_INST['nop']:
            return True
        # 检查 feq.s zero, ft0, ft0
        if self.hex_code == PADDING_INST['feq.s_zero']:
            return True
        # 也可以通过助记符检查
        if self.mnemonic == 'nop':
            return True
        if self.mnemonic == 'feq.s' and 'zero' in self.operands:
            return True
        return False
    
    def _check_is_single_cycle(self) -> bool:
        """检查是否为单周期 ALU 指令"""
        return self.mnemonic in SINGLE_CYCLE_ALU
    
    def _determine_type(self) -> str:
        """确定指令类型"""
        if self.is_nop:
            return 'NOP'
        if self.mnemonic in SINGLE_CYCLE_ALU:
            return 'ALU'
        if self.mnemonic in MULTI_CYCLE_INST:
            if self.mnemonic.startswith('l') or self.mnemonic.startswith('fl'):
                return 'LOAD'
            elif self.mnemonic.startswith('s') or self.mnemonic.startswith('fs'):
                return 'STORE'
            elif self.mnemonic in ['mul', 'mulh', 'mulhsu', 'mulhu', 'div', 'divu', 'rem', 'remu']:
                return 'MULDIV'
            else:
                return 'FPU'
        if self.mnemonic in BRANCH_JUMP_INST:
            return 'BRANCH'
        return 'OTHER'
    
    def _normalize_register(self, reg: str) -> str:
        """标准化寄存器名称（别名转换）"""
        reg = reg.strip()
        # 去除括号（如 0(sp) -> sp）
        if '(' in reg:
            reg = reg.split('(')[1].rstrip(')')
        
        # 转换别名
        if reg in INT_REG_ALIAS:
            return INT_REG_ALIAS[reg]
        if reg in FLOAT_REG_ALIAS:
            return FLOAT_REG_ALIAS[reg]
        return reg
    
    def _parse_operands(self):
        """解析操作数，提取寄存器"""
        if not self.operands or self.is_nop:
            return
        
        # 分割操作数
        parts = [p.strip() for p in self.operands.split(',')]
        
        # 根据指令类型解析
        if self.inst_type == 'ALU':
            # 大多数 ALU 指令格式：rd, rs1, rs2/imm
            if len(parts) >= 1:
                self.rd = self._normalize_register(parts[0])
            if len(parts) >= 2:
                self.rs1 = self._normalize_register(parts[1])
            if len(parts) >= 3:
                # 可能是寄存器或立即数
                if not parts[2].lstrip('-').isdigit() and not parts[2].startswith('0x'):
                    self.rs2 = self._normalize_register(parts[2])
        
        elif self.inst_type == 'LOAD':
            # Load 格式：rd, offset(rs1)
            if len(parts) >= 1:
                self.rd = self._normalize_register(parts[0])
            if len(parts) >= 2:
                # 提取 offset(rs1) 中的 rs1
                if '(' in parts[1]:
                    base_reg = parts[1].split('(')[1].rstrip(')')
                    self.rs1 = self._normalize_register(base_reg)
        
        elif self.inst_type == 'STORE':
            # Store 格式：rs2, offset(rs1)
            if len(parts) >= 1:
                self.rs2 = self._normalize_register(parts[0])
            if len(parts) >= 2:
                if '(' in parts[1]:
                    base_reg = parts[1].split('(')[1].rstrip(')')
                    self.rs1 = self._normalize_register(base_reg)
        
        elif self.inst_type == 'BRANCH':
            # Branch 格式：rs1, rs2, target 或 rd, target
            if self.mnemonic in ['jal', 'jalr', 'call']:
                if len(parts) >= 1:
                    self.rd = self._normalize_register(parts[0])
                if len(parts) >= 2 and self.mnemonic == 'jalr':
                    if '(' in parts[1]:
                        base_reg = parts[1].split('(')[1].rstrip(')')
                        self.rs1 = self._normalize_register(base_reg)
                    else:
                        self.rs1 = self._normalize_register(parts[1])
            elif self.mnemonic in ['ret', 'jr']:
                # ret 隐式使用 ra
                if self.mnemonic == 'ret':
                    self.rs1 = 'x1'  # ra
            else:
                # beq, bne, blt, bge, bltu, bgeu
                if len(parts) >= 1:
                    self.rs1 = self._normalize_register(parts[0])
                if len(parts) >= 2:
                    self.rs2 = self._normalize_register(parts[1])
        
        elif self.inst_type in ['FPU', 'MULDIV']:
            # 通用格式：rd, rs1, rs2[, rs3]
            if len(parts) >= 1:
                self.rd = self._normalize_register(parts[0])
            if len(parts) >= 2:
                self.rs1 = self._normalize_register(parts[1])
            if len(parts) >= 3:
                self.rs2 = self._normalize_register(parts[2])
            if len(parts) >= 4:
                self.rs3 = self._normalize_register(parts[3])
    
    def __repr__(self):
        return f"Inst(0x{self.address:08x}: {self.mnemonic} {self.operands})"


class VLIWPackage:
    """表示一个 VLIW 包（8 条指令）"""
    
    def __init__(self, start_address: int):
        self.start_address = start_address
        self.instructions: List[Instruction] = []
    
    def add_instruction(self, inst: Instruction):
        """添加指令到包中"""
        if len(self.instructions) < 8:
            self.instructions.append(inst)
    
    @property
    def valid_count(self) -> int:
        """有效指令数量（非填充指令）"""
        return sum(1 for inst in self.instructions if not inst.is_nop)
    
    @property
    def is_full(self) -> bool:
        """包是否已满"""
        return len(self.instructions) >= 8
    
    def get_padding_stats(self):
        """获取填充指令统计"""
        if not self.instructions:
            return {'leading': 0, 'trailing': 0, 'middle': 0}
        
        # 包前填充
        leading = 0
        for inst in self.instructions:
            if inst.is_nop:
                leading += 1
            else:
                break
        
        # 包后填充
        trailing = 0
        for inst in reversed(self.instructions):
            if inst.is_nop:
                trailing += 1
            else:
                break
        
        # 包中填充
        total_nop = sum(1 for inst in self.instructions if inst.is_nop)
        middle = total_nop - leading - trailing
        
        return {
            'leading': leading,
            'trailing': trailing,
            'middle': middle
        }
    
    def __repr__(self):
        return f"VLIWPackage(0x{self.start_address:08x}, {len(self.instructions)} insts, {self.valid_count} valid)"

