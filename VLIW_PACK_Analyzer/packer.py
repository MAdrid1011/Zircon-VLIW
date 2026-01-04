"""
VLIW 重打包算法：允许一层依赖的贪心打包
"""

from typing import List, Dict, Set
from instruction import Instruction, VLIWPackage
from dependency import DependencyAnalyzer
from config import VLIW_PACKAGE_SIZE


class VLIWPacker:
    """VLIW 指令重打包优化"""
    
    def __init__(self):
        self.dep_analyzer = DependencyAnalyzer()
    
    def repack_with_one_level_dependency(
        self,
        original_packages: List[VLIWPackage]
    ) -> tuple[List[VLIWPackage], Dict]:
        """
        允许一层依赖的重打包
        
        算法流程：
        1. 提取所有有效指令（非填充）
        2. 构建依赖图
        3. 贪心打包：尽量填满每个包，允许一层依赖
        
        Args:
            original_packages: 原始 VLIW 包列表
            
        Returns:
            (优化后的包列表, 统计信息字典)
        """
        # 1. 提取所有有效指令
        valid_instructions = []
        for pkg in original_packages:
            for inst in pkg.instructions:
                if not inst.is_nop:
                    valid_instructions.append(inst)
        
        if not valid_instructions:
            return [], {'merged_pairs': 0}
        
        # 2. 构建依赖图
        dep_graph = self.dep_analyzer.build_dependency_graph(valid_instructions)
        
        # 3. 贪心打包
        optimized_packages = []
        current_package = VLIWPackage(valid_instructions[0].address if valid_instructions else 0)
        current_package_indices = []  # 当前包中的指令索引
        merged_pairs_count = 0
        
        for i, inst in enumerate(valid_instructions):
            # 检查是否可以加入当前包
            can_add = self._can_add_to_package(
                i, inst, dep_graph, set(range(i)), current_package_indices, valid_instructions
            )
            
            if can_add and not current_package.is_full:
                # 加入当前包
                current_package.add_instruction(inst)
                current_package_indices.append(i)
                
                # 检查是否形成了一层依赖
                for dep_idx in dep_graph.get(i, set()):
                    if dep_idx in current_package_indices:
                        producer = valid_instructions[dep_idx]
                        if self.dep_analyzer.can_form_one_level_dependency(producer, inst):
                            merged_pairs_count += 1
                            break
            else:
                # 当前包已满或不能加入，创建新包
                if current_package.instructions:
                    optimized_packages.append(current_package)
                current_package = VLIWPackage(inst.address)
                current_package_indices = [i]
                current_package.add_instruction(inst)
        
        # 添加最后一个包
        if current_package.instructions:
            optimized_packages.append(current_package)
        
        stats = {
            'merged_pairs': merged_pairs_count
        }
        
        return optimized_packages, stats
    
    def _can_add_to_package(
        self,
        inst_idx: int,
        inst: Instruction,
        dep_graph: Dict[int, Set[int]],
        packed_indices: Set[int],
        current_package_indices: List[int],
        all_instructions: List[Instruction]
    ) -> bool:
        """
        检查指令是否可以加入当前包
        
        条件：
        1. 无依赖 → 可加入
        2. 依赖已在前面的包中（已打包） → 可加入
        3. 依赖在当前包中且形成一层依赖 → 可加入
        4. 否则 → 不可加入
        
        Args:
            inst_idx: 指令索引
            inst: 指令对象
            dep_graph: 依赖图
            packed_indices: 已打包的指令索引集合
            current_package_indices: 当前包中的指令索引列表
            all_instructions: 所有指令列表
            
        Returns:
            是否可以加入
        """
        dependencies = dep_graph.get(inst_idx, set())
        
        # 情况 1：无依赖
        if not dependencies:
            return True
        
        # 检查每个依赖
        for dep_idx in dependencies:
            # 情况 2：依赖已在前面的包中
            if dep_idx in packed_indices and dep_idx not in current_package_indices:
                continue
            
            # 情况 3：依赖在当前包中
            if dep_idx in current_package_indices:
                producer = all_instructions[dep_idx]
                # 检查是否形成一层依赖
                if self.dep_analyzer.can_form_one_level_dependency(producer, inst):
                    continue
                else:
                    # 依赖在当前包但不是一层依赖，不能加入
                    return False
            
            # 依赖还未打包，不能加入
            if dep_idx not in packed_indices:
                return False
        
        return True
    
    def calculate_package_reduction(
        self,
        original_packages: List[VLIWPackage],
        optimized_packages: List[VLIWPackage]
    ) -> Dict:
        """
        计算包数量减少统计
        
        Args:
            original_packages: 原始包列表
            optimized_packages: 优化后的包列表
            
        Returns:
            统计字典
        """
        original_count = len(original_packages)
        optimized_count = len(optimized_packages)
        reduction = original_count - optimized_count
        reduction_percentage = (reduction / original_count * 100) if original_count > 0 else 0
        
        return {
            'original_count': original_count,
            'optimized_count': optimized_count,
            'reduction': reduction,
            'reduction_percentage': reduction_percentage
        }

