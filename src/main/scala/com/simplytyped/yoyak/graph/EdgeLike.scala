package com.simplytyped.yoyak.graph

/** basic edge
  */
trait EdgeLike[Node <: NodeLike[Node]] {
  type L
  val label: L
  val from: Node
  val to: Node

  override def toString: String = {
    s"$from -> $to"
  }
}
