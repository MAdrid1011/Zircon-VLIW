"""
配置文件：指令类型定义和常量
"""

# 单周期 ALU 指令（在 EX1 阶段完成，EX2 阶段可前递）
SINGLE_CYCLE_ALU = {
    # 算术运算
    'add', 'addi', 'sub',
    # 逻辑运算
    'and', 'andi', 'or', 'ori', 'xor', 'xori',
    # 移位运算
    'sll', 'slli', 'srl', 'srli', 'sra', 'srai',
    # 比较运算
    'slt', 'slti', 'sltu', 'sltiu',
    # 立即数加载
    'lui', 'auipc',
    # 伪指令
    'mv', 'li', 'not', 'neg', 'seqz', 'snez', 'sltz', 'sgtz'
}

# 多周期指令（需要 3 级流水或访存）
MULTI_CYCLE_INST = {
    # 乘除法（3 级流水）
    'mul', 'mulh', 'mulhsu', 'mulhu',
    'div', 'divu', 'rem', 'remu',
    # Load/Store（需要访存）
    'lw', 'lh', 'lb', 'lhu', 'lbu', 'flw',
    'sw', 'sh', 'sb', 'fsw',
    # 浮点运算（3 级流水）
    'fadd.s', 'fsub.s', 'fmul.s', 'fdiv.s', 'fsqrt.s',
    'fmadd.s', 'fmsub.s', 'fnmadd.s', 'fnmsub.s',
    'fcvt.w.s', 'fcvt.wu.s', 'fcvt.s.w', 'fcvt.s.wu',
    'fmv.x.w', 'fmv.w.x',
    'feq.s', 'flt.s', 'fle.s',
    'fmin.s', 'fmax.s', 'fsgnj.s', 'fsgnjn.s', 'fsgnjx.s',
    'fclass.s'
}

# 分支/跳转指令
BRANCH_JUMP_INST = {
    'beq', 'bne', 'blt', 'bge', 'bltu', 'bgeu',
    'jal', 'jalr', 'ret', 'j', 'jr', 'call'
}

# 填充指令（十六进制编码）
PADDING_INST = {
    'nop': '00000013',
    'feq.s_zero': 'a0002053'  # feq.s zero, ft0, ft0
}

# 整数寄存器别名映射
INT_REG_ALIAS = {
    'zero': 'x0', 'ra': 'x1', 'sp': 'x2', 'gp': 'x3',
    'tp': 'x4', 't0': 'x5', 't1': 'x6', 't2': 'x7',
    's0': 'x8', 'fp': 'x8', 's1': 'x9',
    'a0': 'x10', 'a1': 'x11', 'a2': 'x12', 'a3': 'x13',
    'a4': 'x14', 'a5': 'x15', 'a6': 'x16', 'a7': 'x17',
    's2': 'x18', 's3': 'x19', 's4': 'x20', 's5': 'x21',
    's6': 'x22', 's7': 'x23', 's8': 'x24', 's9': 'x25',
    's10': 'x26', 's11': 'x27',
    't3': 'x28', 't4': 'x29', 't5': 'x30', 't6': 'x31'
}

# 浮点寄存器别名映射
FLOAT_REG_ALIAS = {
    'ft0': 'f0', 'ft1': 'f1', 'ft2': 'f2', 'ft3': 'f3',
    'ft4': 'f4', 'ft5': 'f5', 'ft6': 'f6', 'ft7': 'f7',
    'fs0': 'f8', 'fs1': 'f9',
    'fa0': 'f10', 'fa1': 'f11', 'fa2': 'f12', 'fa3': 'f13',
    'fa4': 'f14', 'fa5': 'f15', 'fa6': 'f16', 'fa7': 'f17',
    'fs2': 'f18', 'fs3': 'f19', 'fs4': 'f20', 'fs5': 'f21',
    'fs6': 'f22', 'fs7': 'f23', 'fs8': 'f24', 'fs9': 'f25',
    'fs10': 'f26', 'fs11': 'f27',
    'ft8': 'f28', 'ft9': 'f29', 'ft10': 'f30', 'ft11': 'f31'
}

# VLIW 包大小
VLIW_PACKAGE_SIZE = 8

