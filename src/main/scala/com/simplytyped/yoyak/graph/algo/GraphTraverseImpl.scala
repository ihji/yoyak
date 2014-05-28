package com.simplytyped.yoyak.graph.algo

import com.simplytyped.yoyak.graph.{NodeLike, EdgeLike, GraphLike}

trait GraphTraverseImpl[Node <: NodeLike[Node], Edge <: EdgeLike[Node], Graph <: GraphLike[Node,Edge,Graph]] extends GraphTraverse[Graph,Node] {
  def depthFirstTraverse(g: Graph) : Stream[Node] = {
    def depthFirstTraverse_aux(visiting: Node, visited: Set[Node], notVisited: List[Node]) : Stream[Node] = {
      val nexts = g.getNexts(visiting).toList.sorted
      val newNotVisited  = (nexts ++ notVisited).dropWhile(visited)
      if(newNotVisited.isEmpty) visiting #:: Stream.empty[Node]
      else visiting #:: depthFirstTraverse_aux(newNotVisited.head,visited+visiting,newNotVisited.tail)
    }
    val roots = (g.nexts.keySet -- g.prevs.keySet).toList.sorted
    if(roots.isEmpty) Stream.empty[Node]
    else depthFirstTraverse_aux(roots.head,Set(),roots.tail)
  }
  def findLoopheads(g: Graph) : List[Node] = {
    def findLoopheads_aux(visiting: Node, visited: Set[Node]) : List[Node] = {
      val nexts = g.getNexts(visiting).toList.sorted
      nexts.foldLeft(List.empty[Node]) {
        case (l,n) =>
          if(visited(n)) n::l
          else findLoopheads_aux(n, visited + visiting) ++ l
      }
    }
    val roots = (g.nexts.keySet -- g.prevs.keySet).toList.sorted
    if(roots.isEmpty) List.empty[Node]
    else roots.foldLeft(List.empty[Node]) {
      case (l,n) => findLoopheads_aux(n,Set())
    }.distinct
  }
}