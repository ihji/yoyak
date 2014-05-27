package com.simplytyped.yoyak.graph.algo

import com.simplytyped.yoyak.graph.{NodeLike, EdgeLike, GraphLike}

class GraphSearchImpl[Node <: NodeLike[Node], Edge <: EdgeLike[Node], Graph <: GraphLike[Node,Edge,Graph]] extends GraphSearch[Graph,Node] {
  def findLoopheads(g: Graph) : List[Node] = {
    ???
  }
}
