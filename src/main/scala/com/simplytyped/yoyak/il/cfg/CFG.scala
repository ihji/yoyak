package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.ImmutableGraphLike

case class CFG(nodes: Set[BasicBlock], edges: Set[BasicEdge], nexts: Map[BasicBlock,Set[BasicEdge]], prevs: Map[BasicBlock,Set[BasicEdge]]) extends ImmutableGraphLike[BasicBlock,BasicEdge,CFG] {
  def newEdge(from: BasicBlock, to: BasicBlock): BasicEdge = BasicEdge(from,to)
  def builder(nodes: Set[BasicBlock], edges: Set[BasicEdge], nexts: Map[BasicBlock, Set[BasicEdge]], prevs: Map[BasicBlock, Set[BasicEdge]]): CFG = {
    CFG(nodes,edges,nexts,prevs)
  }
}

object CFG {
  val empty = CFG(Set.empty,Set.empty,Map.empty,Map.empty)
}