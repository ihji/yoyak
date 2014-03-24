package com.simplytyped.yoyak.graph

/**
 * basic node (vertex)
 */
trait NodeLike {
  type D
  val data : D
  override def toString : String = {
    s"$data"
  }
}
