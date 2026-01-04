#!/usr/bin/env python3
"""
VLIW 反汇编分析工具 - 命令行入口

用法:
    python main.py <input_file> [options]
    
示例:
    python main.py ../VLIW_PACK/FFT/build/FFT-riscv32.txt
    python main.py FFT-riscv32.txt --output report.txt
    python main.py FFT-riscv32.txt --verbose
"""

import sys
import argparse
import os
from analyzer import VLIWAnalyzer


def main():
    """命令行入口"""
    parser = argparse.ArgumentParser(
        description='VLIW 反汇编分析工具',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
示例:
  python main.py ../VLIW_PACK/FFT/build/FFT-riscv32.txt
  python main.py FFT-riscv32.txt --output report.txt
  python main.py FFT-riscv32.txt --verbose
        """
    )
    
    parser.add_argument(
        'input_file',
        help='反汇编文件路径'
    )
    
    parser.add_argument(
        '--output', '-o',
        help='输出报告文件路径（默认：标准输出）',
        default=None
    )
    
    parser.add_argument(
        '--verbose', '-v',
        help='显示详细输出（包括指令类型分布）',
        action='store_true'
    )
    
    parser.add_argument(
        '--export-asm', '-e',
        help='导出重排后的反汇编文件路径',
        default=None
    )
    
    args = parser.parse_args()
    
    # 检查输入文件是否存在
    if not os.path.exists(args.input_file):
        print(f"错误：文件不存在: {args.input_file}", file=sys.stderr)
        return 1
    
    try:
        # 创建分析器
        analyzer = VLIWAnalyzer(args.input_file)
        
        # 运行分析
        analyzer.run_full_analysis()
        
        # 生成报告
        if args.output:
            analyzer.save_report(args.output, verbose=args.verbose)
        else:
            report = analyzer.generate_report(verbose=args.verbose)
            print(report)
        
        # 导出重排后的反汇编
        if args.export_asm:
            analyzer.export_reordered_asm(args.export_asm)
        
        return 0
    
    except Exception as e:
        print(f"错误：分析过程中出现异常: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())

