package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.EdgeLike

case class BasicEdge(from: BasicBlock, to: BasicBlock) extends EdgeLike[BasicBlock] {
  type L = Option[Nothing]
  val label = None
}
