package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.ImmutableGraphLike
import com.simplytyped.yoyak.graph.algo.GraphTraverseImpl

case class CFG(
    nodes: Set[BasicBlock],
    edges: Set[BasicEdge],
    nexts: Map[BasicBlock, Set[BasicEdge]],
    prevs: Map[BasicBlock, Set[BasicEdge]]
) extends ImmutableGraphLike[BasicBlock, BasicEdge, CFG] {
  def newEdge(from: BasicBlock, to: BasicBlock): BasicEdge = BasicEdge(from, to)
  def builder(
      nodes: Set[BasicBlock],
      edges: Set[BasicEdge],
      nexts: Map[BasicBlock, Set[BasicEdge]],
      prevs: Map[BasicBlock, Set[BasicEdge]]
  ): CFG = {
    CFG(nodes, edges, nexts, prevs)
  }
  def getEntry: Option[BasicBlock] = nodes.find { _.isEntry }
  def getExit: Option[BasicBlock]  = nodes.find { _.isExit }
  def findBasicBlockByLineNumber(lineNumber: Int): Option[BasicBlock] = {
    nodes.find { _.data.getStmts.exists { _.pos.startLine == lineNumber } }
  }
}

object CFG {
  val empty    = CFG(Set.empty, Set.empty, Map.empty, Map.empty)
  val traverse = new GraphTraverseImpl[BasicBlock, BasicEdge, CFG] {}
}
