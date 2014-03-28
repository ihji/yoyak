package com.simplytyped.yoyak.graph.algo

abstract class GraphRefactoring[G,N] {
  def mergePairedNodes(merger: (N,N)=>N)(g: G) : G
}
