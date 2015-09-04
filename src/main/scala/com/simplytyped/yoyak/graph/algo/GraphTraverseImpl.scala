package com.simplytyped.yoyak.graph.algo

import com.simplytyped.yoyak.graph.{NodeLike, EdgeLike, GraphLike}

trait GraphTraverseImpl[Node <: NodeLike[Node], Edge <: EdgeLike[Node], Graph <: GraphLike[Node,Edge,Graph]] extends GraphTraverse[Graph,Node] {
  def depthFirstTraverse(g: Graph) : Stream[Node] = {
    def depthFirstTraverse_aux(visiting: Node, visited: Set[Node], notVisited: List[Node]) : Stream[Node] = {
      val nexts = g.getNexts(visiting).toList
      val newNotVisited  = (nexts ++ notVisited).dropWhile(visited)
      if(newNotVisited.isEmpty) visiting #:: Stream.empty[Node]
      else visiting #:: depthFirstTraverse_aux(newNotVisited.head,visited+visiting,newNotVisited.tail)
    }
    val roots = (g.nexts.keySet -- g.prevs.keySet).toList
    if(roots.isEmpty) Stream.empty[Node]
    else depthFirstTraverse_aux(roots.head,Set(),roots.tail)
  }
  def findLoopheads(g: Graph) : Vector[Node] = {
    def findLoopheads_aux(visiting: Node, visited: Set[Node]) : Vector[Node] = {
      val nexts = g.getNexts(visiting).toList
      nexts.foldLeft(Vector.empty[Node]) {
        case (l,n) =>
          if(visited(n)) l :+ n
          else l ++ findLoopheads_aux(n, visited + visiting)
      }
    }
    val roots = (g.nexts.keySet -- g.prevs.keySet).toList
    if(roots.isEmpty) Vector.empty[Node]
    else roots.foldLeft(Vector.empty[Node]) {
      case (l,n) => l ++ findLoopheads_aux(n,Set())
    }.distinct
  }
}