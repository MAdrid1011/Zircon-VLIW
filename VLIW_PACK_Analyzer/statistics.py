"""
统计与报告生成：收集和生成分析报告
"""

from typing import List, Dict
from instruction import VLIWPackage


class StatisticsCollector:
    """收集和生成统计报告"""
    
    def analyze_original_packages(self, packages: List[VLIWPackage]) -> Dict:
        """
        分析原始包统计
        
        Args:
            packages: VLIW 包列表
            
        Returns:
            统计字典
        """
        total_packages = len(packages)
        total_instructions = sum(len(pkg.instructions) for pkg in packages)
        valid_instructions = sum(pkg.valid_count for pkg in packages)
        padding_instructions = total_instructions - valid_instructions
        
        # 统计不同类型的填充指令
        nop_count = 0
        feq_zero_count = 0
        
        for pkg in packages:
            for inst in pkg.instructions:
                if inst.is_nop:
                    if inst.hex_code == '00000013' or inst.mnemonic == 'nop':
                        nop_count += 1
                    elif inst.hex_code == 'a0002053':
                        feq_zero_count += 1
        
        avg_valid_per_package = valid_instructions / total_packages if total_packages > 0 else 0
        
        return {
            'total_packages': total_packages,
            'total_instructions': total_instructions,
            'valid_instructions': valid_instructions,
            'padding_instructions': padding_instructions,
            'nop_count': nop_count,
            'feq_zero_count': feq_zero_count,
            'avg_valid_per_package': avg_valid_per_package,
            'valid_percentage': (valid_instructions / total_instructions * 100) if total_instructions > 0 else 0
        }
    
    def analyze_padding_instructions(self, packages: List[VLIWPackage]) -> Dict:
        """
        分析填充指令统计
        
        Args:
            packages: VLIW 包列表
            
        Returns:
            填充指令统计字典
        """
        leading_total = 0
        trailing_total = 0
        middle_total = 0
        
        for pkg in packages:
            stats = pkg.get_padding_stats()
            leading_total += stats['leading']
            trailing_total += stats['trailing']
            middle_total += stats['middle']
        
        total_padding = leading_total + trailing_total + middle_total
        removable_padding = leading_total + trailing_total
        
        # 计算程序大小（每条指令 4 字节）
        total_instructions = sum(len(pkg.instructions) for pkg in packages)
        original_size = total_instructions * 4
        optimized_size = (total_instructions - removable_padding) * 4
        size_reduction = original_size - optimized_size
        reduction_percentage = (size_reduction / original_size * 100) if original_size > 0 else 0
        
        return {
            'leading_padding': leading_total,
            'trailing_padding': trailing_total,
            'middle_padding': middle_total,
            'total_padding': total_padding,
            'removable_padding': removable_padding,
            'original_size_bytes': original_size,
            'optimized_size_bytes': optimized_size,
            'size_reduction_bytes': size_reduction,
            'reduction_percentage': reduction_percentage
        }
    
    def compare_packing_results(
        self,
        original_packages: List[VLIWPackage],
        optimized_packages: List[VLIWPackage]
    ) -> Dict:
        """
        对比打包结果
        
        Args:
            original_packages: 原始 VLIW 包列表
            optimized_packages: 优化后的 VLIW 包列表
            
        Returns:
            对比统计字典
        """
        original_count = len(original_packages)
        optimized_count = len(optimized_packages)
        package_reduction = original_count - optimized_count
        reduction_percentage = (package_reduction / original_count * 100) if original_count > 0 else 0
        
        # 计算有效指令数（应该相同）
        original_valid = sum(pkg.valid_count for pkg in original_packages)
        optimized_valid = sum(pkg.valid_count for pkg in optimized_packages)
        
        # 计算平均每包有效指令数
        original_avg = original_valid / original_count if original_count > 0 else 0
        optimized_avg = optimized_valid / optimized_count if optimized_count > 0 else 0
        
        # 计算指令密度提升
        density_improvement = ((optimized_avg - original_avg) / original_avg * 100) if original_avg > 0 else 0
        
        return {
            'original_package_count': original_count,
            'optimized_package_count': optimized_count,
            'package_reduction': package_reduction,
            'reduction_percentage': reduction_percentage,
            'original_valid_instructions': original_valid,
            'optimized_valid_instructions': optimized_valid,
            'original_avg_density': original_avg,
            'optimized_avg_density': optimized_avg,
            'density_improvement': density_improvement
        }
    
    def analyze_instruction_types(self, packages: List[VLIWPackage]) -> Dict:
        """
        分析指令类型分布
        
        Args:
            packages: VLIW 包列表
            
        Returns:
            指令类型统计字典
        """
        type_counts = {}
        total_valid = 0
        
        for pkg in packages:
            for inst in pkg.instructions:
                if not inst.is_nop:
                    total_valid += 1
                    inst_type = inst.inst_type
                    type_counts[inst_type] = type_counts.get(inst_type, 0) + 1
        
        # 计算百分比
        type_percentages = {}
        for inst_type, count in type_counts.items():
            percentage = (count / total_valid * 100) if total_valid > 0 else 0
            type_percentages[inst_type] = {
                'count': count,
                'percentage': percentage
            }
        
        return {
            'total_valid_instructions': total_valid,
            'type_distribution': type_percentages
        }
    
    def generate_report(
        self,
        original_stats: Dict,
        padding_stats: Dict,
        type_stats: Dict = None,
        packing_stats: Dict = None,
        dependency_stats: Dict = None
    ) -> str:
        """
        生成可读的分析报告
        
        Args:
            original_stats: 原始包统计
            padding_stats: 填充指令统计
            type_stats: 指令类型统计（可选）
            packing_stats: 重打包对比统计（可选）
            dependency_stats: 依赖关系统计（可选）
            
        Returns:
            格式化的报告字符串
        """
        lines = []
        lines.append("=" * 60)
        lines.append("VLIW 反汇编分析报告")
        lines.append("=" * 60)
        lines.append("")
        
        # 原始包统计
        lines.append("--- 原始包统计 ---")
        lines.append(f"总包数：{original_stats['total_packages']}")
        lines.append(f"总指令数：{original_stats['total_instructions']} ({original_stats['total_packages']} × 8)")
        lines.append(f"有效指令数：{original_stats['valid_instructions']} ({original_stats['valid_percentage']:.1f}%)")
        lines.append(f"填充指令数：{original_stats['padding_instructions']} ({100 - original_stats['valid_percentage']:.1f}%)")
        lines.append(f"  - nop (0x00000013): {original_stats['nop_count']} 条")
        lines.append(f"  - feq.s zero (0xa0002053): {original_stats['feq_zero_count']} 条")
        lines.append(f"平均每包有效指令：{original_stats['avg_valid_per_package']:.2f}")
        lines.append("")
        
        # 填充指令分析
        lines.append("--- 填充指令分析 ---")
        total_padding = padding_stats['total_padding']
        lines.append(f"包前填充：{padding_stats['leading_padding']} 条 ({padding_stats['leading_padding']/total_padding*100:.1f}%)" if total_padding > 0 else "包前填充：0 条")
        lines.append(f"包后填充：{padding_stats['trailing_padding']} 条 ({padding_stats['trailing_padding']/total_padding*100:.1f}%)" if total_padding > 0 else "包后填充：0 条")
        lines.append(f"包中填充：{padding_stats['middle_padding']} 条 ({padding_stats['middle_padding']/total_padding*100:.1f}%)" if total_padding > 0 else "包中填充：0 条")
        lines.append(f"可删除填充：{padding_stats['removable_padding']} 条 (包前 + 包后)")
        lines.append(f"程序大小：{padding_stats['original_size_bytes']} 字节 ({original_stats['total_instructions']} × 4)")
        lines.append(f"优化后大小：{padding_stats['optimized_size_bytes']} 字节")
        lines.append(f"程序大小缩减：{padding_stats['size_reduction_bytes']} 字节 ({padding_stats['reduction_percentage']:.1f}%)")
        lines.append("")
        
        # 指令类型分布
        if type_stats:
            lines.append("--- 指令类型分布 ---")
            type_dist = type_stats['type_distribution']
            for inst_type in sorted(type_dist.keys()):
                info = type_dist[inst_type]
                lines.append(f"{inst_type}: {info['count']} 条 ({info['percentage']:.1f}%)")
            lines.append("")
        
        # 重打包对比
        if packing_stats:
            lines.append("--- 一层依赖重打包分析 ---")
            lines.append(f"优化后包数：{packing_stats['optimized_package_count']}")
            lines.append(f"包数量减少：{packing_stats['package_reduction']} ({packing_stats['reduction_percentage']:.1f}%)")
            lines.append(f"有效指令数：{packing_stats['optimized_valid_instructions']} (不变)")
            lines.append(f"平均每包有效指令：{packing_stats['optimized_avg_density']:.2f}")
            lines.append(f"指令密度提升：{packing_stats['density_improvement']:.1f}%")
            
            if dependency_stats:
                lines.append("")
                lines.append(f"单周期 ALU 指令：{dependency_stats.get('single_cycle_count', 0)} 条")
                lines.append(f"可形成一层依赖对：{dependency_stats.get('one_level_pairs', 0)} 对")
                lines.append(f"成功合并到同一包：{dependency_stats.get('merged_pairs', 0)} 对")
            lines.append("")
        
        # 依赖关系统计
        if dependency_stats and not packing_stats:
            lines.append("--- 依赖关系统计 ---")
            lines.append(f"无依赖指令：{dependency_stats.get('independent_count', 0)} 条")
            lines.append(f"有依赖指令：{dependency_stats.get('dependent_count', 0)} 条")
            lines.append("")
        
        lines.append("=" * 60)
        lines.append("分析完成")
        lines.append("=" * 60)
        
        return "\n".join(lines)

