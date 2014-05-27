package com.simplytyped.yoyak.graph.algo

abstract class GraphTraverse[G,N] {
  def depthFirstTraverse(g: G) : Stream[N]
}
