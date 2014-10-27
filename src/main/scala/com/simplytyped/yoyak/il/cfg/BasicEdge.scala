package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.EdgeLike
import com.simplytyped.yoyak.il.cfg.BasicEdge.{IntraEdge, EdgeType}

case class BasicEdge(from: BasicBlock, to: BasicBlock, label: EdgeType = IntraEdge) extends EdgeLike[BasicBlock] {
  type L = EdgeType
}

object BasicEdge {
  abstract class EdgeType
  case object InterEdge extends EdgeType
  case object IntraEdge extends EdgeType
}