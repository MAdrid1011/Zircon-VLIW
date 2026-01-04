"""
主分析器：协调各模块完成完整分析流程
"""

import os
from typing import Dict
from parser import DisassemblyParser
from dependency import DependencyAnalyzer
from packer import VLIWPacker
from statistics import StatisticsCollector


class VLIWAnalyzer:
    """主分析器，协调各模块"""
    
    def __init__(self, filepath: str):
        """
        初始化分析器
        
        Args:
            filepath: 反汇编文件路径
        """
        self.filepath = filepath
        self.filename = os.path.basename(filepath)
        
        # 初始化各模块
        self.parser = DisassemblyParser()
        self.dep_analyzer = DependencyAnalyzer()
        self.packer = VLIWPacker()
        self.stats_collector = StatisticsCollector()
        
        # 数据存储
        self.original_packages = []
        self.optimized_packages = []
        self.dep_graph = {}
        self.all_stats = {}
    
    def run_full_analysis(self) -> Dict:
        """
        运行完整分析流程
        
        Returns:
            所有统计数据的字典
        """
        print(f"正在分析文件: {self.filename}")
        print()
        
        # 1. 解析反汇编文件
        print("[1/6] 解析反汇编文件...")
        self.original_packages = self.parser.parse_file(self.filepath)
        print(f"  解析完成：{len(self.original_packages)} 个 VLIW 包")
        
        # 2. 分析原始包统计
        print("[2/6] 分析原始包统计...")
        original_stats = self.stats_collector.analyze_original_packages(self.original_packages)
        self.all_stats['original'] = original_stats
        print(f"  有效指令：{original_stats['valid_instructions']} / {original_stats['total_instructions']}")
        
        # 3. 分析填充指令
        print("[3/6] 分析填充指令...")
        padding_stats = self.stats_collector.analyze_padding_instructions(self.original_packages)
        self.all_stats['padding'] = padding_stats
        print(f"  可删除填充：{padding_stats['removable_padding']} 条")
        
        # 4. 分析指令类型分布
        print("[4/6] 分析指令类型分布...")
        type_stats = self.stats_collector.analyze_instruction_types(self.original_packages)
        self.all_stats['types'] = type_stats
        
        # 5. 构建依赖图并分析
        print("[5/6] 构建依赖图...")
        # 提取所有有效指令
        valid_instructions = []
        for pkg in self.original_packages:
            for inst in pkg.instructions:
                if not inst.is_nop:
                    valid_instructions.append(inst)
        
        self.dep_graph = self.dep_analyzer.build_dependency_graph(valid_instructions)
        dependency_stats = self.dep_analyzer.analyze_dependency_statistics(
            valid_instructions, self.dep_graph
        )
        self.all_stats['dependency'] = dependency_stats
        print(f"  一层依赖对：{dependency_stats['one_level_pairs']} 对")
        
        # 6. 重打包（允许一层依赖）
        print("[6/6] 重打包分析...")
        self.optimized_packages, repack_stats = self.packer.repack_with_one_level_dependency(
            self.original_packages
        )
        
        # 合并重打包统计
        packing_stats = self.stats_collector.compare_packing_results(
            self.original_packages,
            self.optimized_packages
        )
        packing_stats['merged_pairs'] = repack_stats['merged_pairs']
        self.all_stats['packing'] = packing_stats
        print(f"  优化后包数：{packing_stats['optimized_package_count']}")
        print()
        
        return self.all_stats
    
    def generate_report(self, verbose: bool = False) -> str:
        """
        生成分析报告
        
        Args:
            verbose: 是否生成详细报告
            
        Returns:
            报告字符串
        """
        if not self.all_stats:
            return "错误：尚未运行分析，请先调用 run_full_analysis()"
        
        # 准备依赖统计（合并到 packing 中）
        dependency_stats = {
            'single_cycle_count': self.all_stats['dependency']['single_cycle_count'],
            'one_level_pairs': self.all_stats['dependency']['one_level_pairs'],
            'merged_pairs': self.all_stats['packing']['merged_pairs']
        }
        
        report = self.stats_collector.generate_report(
            original_stats=self.all_stats['original'],
            padding_stats=self.all_stats['padding'],
            type_stats=self.all_stats['types'] if verbose else None,
            packing_stats=self.all_stats['packing'],
            dependency_stats=dependency_stats
        )
        
        # 添加文件名
        lines = report.split('\n')
        lines.insert(2, f"文件：{self.filename}")
        lines.insert(3, "")
        
        return '\n'.join(lines)
    
    def save_report(self, output_path: str, verbose: bool = False):
        """
        保存报告到文件
        
        Args:
            output_path: 输出文件路径
            verbose: 是否生成详细报告
        """
        report = self.generate_report(verbose)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(report)
        
        print(f"报告已保存到: {output_path}")

