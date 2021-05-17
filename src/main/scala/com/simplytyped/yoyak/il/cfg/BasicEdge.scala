package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.EdgeLike
import com.simplytyped.yoyak.il.CommonIL.Value
import com.simplytyped.yoyak.il.cfg.BasicEdge.{IntraEdge, EdgeType}

case class BasicEdge(
    from: BasicBlock,
    to: BasicBlock,
    label: EdgeType = IntraEdge
) extends EdgeLike[BasicBlock] {
  type L = EdgeType
}

object BasicEdge {
  abstract class EdgeType
  case class InterEdge(retVar: Option[Value.Local]) extends EdgeType
  case object IntraEdge                             extends EdgeType
}
