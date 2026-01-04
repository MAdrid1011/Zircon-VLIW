"""
依赖关系分析：分析指令间的数据依赖关系
"""

from typing import List, Dict, Set, Tuple
from instruction import Instruction, VLIWPackage


class DependencyAnalyzer:
    """分析指令间的数据依赖关系"""
    
    def __init__(self):
        pass
    
    def analyze_raw_dependency(self, producer: Instruction, consumer: Instruction) -> bool:
        """
        检查 consumer 是否依赖 producer（RAW 依赖）
        
        Args:
            producer: 生产者指令
            consumer: 消费者指令
            
        Returns:
            是否存在 RAW 依赖
        """
        if not producer.rd:
            return False
        
        # 特殊处理：写入 x0/zero 寄存器无效
        if producer.rd == 'x0':
            return False
        
        # 检查 consumer 的源寄存器是否与 producer 的目标寄存器相同
        consumer_sources = []
        if consumer.rs1:
            consumer_sources.append(consumer.rs1)
        if consumer.rs2:
            consumer_sources.append(consumer.rs2)
        if consumer.rs3:
            consumer_sources.append(consumer.rs3)
        
        return producer.rd in consumer_sources
    
    def can_form_one_level_dependency(self, producer: Instruction, consumer: Instruction) -> bool:
        """
        检查是否可以形成一层依赖
        
        一层依赖的条件：
        1. consumer 依赖 producer
        2. producer 和 consumer 都是单周期 ALU 指令
        
        Args:
            producer: 生产者指令
            consumer: 消费者指令
            
        Returns:
            是否可以形成一层依赖
        """
        # 两者都必须是单周期 ALU 指令
        if not (producer.is_single_cycle and consumer.is_single_cycle):
            return False
        
        # consumer 必须依赖 producer
        if not self.analyze_raw_dependency(producer, consumer):
            return False
        
        return True
    
    def build_dependency_graph(self, instructions: List[Instruction]) -> Dict[int, Set[int]]:
        """
        构建依赖图
        
        Args:
            instructions: 指令列表
            
        Returns:
            依赖图字典，key 为指令索引，value 为其依赖的指令索引集合
        """
        dep_graph = {}
        
        for i, consumer in enumerate(instructions):
            if consumer.is_nop:
                dep_graph[i] = set()
                continue
            
            dependencies = set()
            
            # 向前查找依赖（只需要查找最近的写入）
            for j in range(i - 1, -1, -1):
                producer = instructions[j]
                
                if producer.is_nop:
                    continue
                
                # 检查是否存在 RAW 依赖
                if self.analyze_raw_dependency(producer, consumer):
                    dependencies.add(j)
                    # 找到最近的写入后，对于该寄存器不再继续查找
                    # 但需要检查其他源寄存器
            
            dep_graph[i] = dependencies
        
        return dep_graph
    
    def find_one_level_dependency_pairs(
        self,
        instructions: List[Instruction],
        dep_graph: Dict[int, Set[int]]
    ) -> List[Tuple[int, int]]:
        """
        找出所有可以形成一层依赖的指令对
        
        Args:
            instructions: 指令列表
            dep_graph: 依赖图
            
        Returns:
            一层依赖对列表 [(producer_idx, consumer_idx), ...]
        """
        one_level_pairs = []
        
        for consumer_idx, producer_indices in dep_graph.items():
            consumer = instructions[consumer_idx]
            
            if consumer.is_nop:
                continue
            
            for producer_idx in producer_indices:
                producer = instructions[producer_idx]
                
                if self.can_form_one_level_dependency(producer, consumer):
                    one_level_pairs.append((producer_idx, consumer_idx))
        
        return one_level_pairs
    
    def analyze_dependency_statistics(
        self,
        instructions: List[Instruction],
        dep_graph: Dict[int, Set[int]]
    ) -> Dict:
        """
        分析依赖关系统计
        
        Args:
            instructions: 指令列表
            dep_graph: 依赖图
            
        Returns:
            统计字典
        """
        valid_instructions = [inst for inst in instructions if not inst.is_nop]
        total_valid = len(valid_instructions)
        
        # 统计单周期 ALU 指令
        single_cycle_count = sum(1 for inst in valid_instructions if inst.is_single_cycle)
        
        # 统计无依赖和有依赖的指令
        independent_count = 0
        dependent_count = 0
        
        for i, inst in enumerate(instructions):
            if inst.is_nop:
                continue
            
            if len(dep_graph.get(i, set())) == 0:
                independent_count += 1
            else:
                dependent_count += 1
        
        # 找出一层依赖对
        one_level_pairs = self.find_one_level_dependency_pairs(instructions, dep_graph)
        
        return {
            'total_valid_instructions': total_valid,
            'single_cycle_count': single_cycle_count,
            'independent_count': independent_count,
            'dependent_count': dependent_count,
            'one_level_pairs': len(one_level_pairs),
            'one_level_pair_list': one_level_pairs
        }

