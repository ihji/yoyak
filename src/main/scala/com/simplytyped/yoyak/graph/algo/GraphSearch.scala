package com.simplytyped.yoyak.graph.algo

abstract class GraphSearch[G,N] {
  def findLoopheads(g: G) : List[N]
}
