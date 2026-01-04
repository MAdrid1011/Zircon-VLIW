#!/usr/bin/env python3
"""
测试 Branch 指令参与一层依赖计算 & 重排反汇编输出
"""

import sys
import os
import tempfile

# 添加父目录到路径
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from instruction import Instruction
from dependency import DependencyAnalyzer
from config import BRANCH_JUMP_INST, ONE_LEVEL_DEPENDENCY_ELIGIBLE


def test_branch_in_one_level_dependency_eligible():
    """测试 Branch 指令在 ONE_LEVEL_DEPENDENCY_ELIGIBLE 集合中"""
    print("测试 1: Branch 指令在 ONE_LEVEL_DEPENDENCY_ELIGIBLE 集合中")
    
    for branch_inst in BRANCH_JUMP_INST:
        assert branch_inst in ONE_LEVEL_DEPENDENCY_ELIGIBLE, \
            f"Branch 指令 {branch_inst} 应该在 ONE_LEVEL_DEPENDENCY_ELIGIBLE 中"
    
    print("  ✓ 所有 Branch 指令都在 ONE_LEVEL_DEPENDENCY_ELIGIBLE 中")


def test_branch_instruction_can_one_level_dep():
    """测试 Branch 指令的 can_one_level_dep 属性"""
    print("测试 2: Branch 指令的 can_one_level_dep 属性")
    
    # 创建几个 Branch 指令
    branch_insts = [
        Instruction(0x80000000, "00000063", "beq", "a0, a1, 0x80000010"),
        Instruction(0x80000004, "00000067", "jalr", "ra, 0(ra)"),
        Instruction(0x80000008, "0000006f", "jal", "ra, 0x80000100"),
        Instruction(0x8000000c, "00008067", "ret", ""),
    ]
    
    for inst in branch_insts:
        assert inst.can_one_level_dep, \
            f"Branch 指令 {inst.mnemonic} 的 can_one_level_dep 应该为 True"
    
    print("  ✓ 所有 Branch 指令的 can_one_level_dep 属性正确")


def test_alu_branch_one_level_dependency():
    """测试 ALU 指令与 Branch 指令形成一层依赖"""
    print("测试 3: ALU 与 Branch 指令形成一层依赖")
    
    dep_analyzer = DependencyAnalyzer()
    
    # 场景：li a0, 1 → beq a0, zero, target
    producer = Instruction(0x80000000, "00100513", "li", "a0, 0x1")
    consumer = Instruction(0x80000004, "00050063", "beq", "a0, zero, 0x80000010")
    
    # producer 是单周期 ALU，consumer 可以参与一层依赖
    assert producer.is_single_cycle, "li 应该是单周期 ALU 指令"
    assert consumer.can_one_level_dep, "beq 应该可以参与一层依赖"
    
    # 检查是否形成一层依赖
    can_form = dep_analyzer.can_form_one_level_dependency(producer, consumer)
    assert can_form, "li a0 → beq a0 应该形成一层依赖"
    
    print("  ✓ ALU 与 Branch 指令正确形成一层依赖")


def test_export_reordered_asm():
    """测试重排反汇编导出功能"""
    print("测试 4: 重排反汇编导出功能")
    
    from analyzer import VLIWAnalyzer
    
    # 使用实际的测试文件
    test_file = os.path.join(
        os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
        "VLIW_PACK/FFT/build/FFT-riscv32.txt"
    )
    
    if not os.path.exists(test_file):
        print(f"  ⚠ 测试文件不存在: {test_file}，跳过此测试")
        return
    
    # 创建临时输出文件
    with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
        output_path = f.name
    
    try:
        # 运行分析
        analyzer = VLIWAnalyzer(test_file)
        analyzer.run_full_analysis()
        
        # 导出重排后的反汇编
        analyzer.export_reordered_asm(output_path)
        
        # 验证输出文件存在且非空
        assert os.path.exists(output_path), "输出文件应该存在"
        with open(output_path, 'r') as f:
            content = f.read()
        assert len(content) > 0, "输出文件不应为空"
        assert "# VLIW 重排后反汇编" in content, "输出文件应包含头部注释"
        assert "# === Package" in content, "输出文件应包含包边界注释"
        
        print("  ✓ 重排反汇编导出功能正常")
    
    finally:
        # 清理临时文件
        if os.path.exists(output_path):
            os.remove(output_path)


def main():
    """运行所有测试"""
    print("=" * 60)
    print("Branch 指令一层依赖 & 重排反汇编 测试")
    print("=" * 60)
    print()
    
    tests = [
        test_branch_in_one_level_dependency_eligible,
        test_branch_instruction_can_one_level_dep,
        test_alu_branch_one_level_dependency,
        test_export_reordered_asm,
    ]
    
    passed = 0
    failed = 0
    
    for test in tests:
        try:
            test()
            passed += 1
        except AssertionError as e:
            print(f"  ✗ 失败: {e}")
            failed += 1
        except Exception as e:
            print(f"  ✗ 错误: {e}")
            failed += 1
        print()
    
    print("=" * 60)
    print(f"测试结果: {passed} 通过, {failed} 失败")
    print("=" * 60)
    
    return 0 if failed == 0 else 1


if __name__ == '__main__':
    sys.exit(main())

