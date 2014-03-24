package com.simplytyped.yoyak.graph

/**
 * trait for immutable graphs
 */
trait ImmutableGraphLike[Node <: NodeLike, Edge <: EdgeLike[Node], Graph <: GraphLike[Node,Edge,Graph]] extends GraphLike[Node,Edge,Graph] { self: Graph =>
  protected val nexts : Map[Node,Set[Edge]]
  protected val prevs : Map[Node,Set[Edge]]

  def addEdge(from: Node, to: Node) : Graph = {
    val edge = newEdge(from, to)
    val newNodes = nodes ++ Seq(from,to)
    val newEdges = edges + edge
    val newNexts = nexts + (from->(nexts.getOrElse(from,Set.empty)+edge))
    val newPrevs = prevs + (to->(prevs.getOrElse(to,Set.empty)+edge))
    builder(newNodes,newEdges,newNexts,newPrevs)
  }
  def addEdge(e: Edge) : Graph = {
    val newNodes = nodes ++ Seq(e.from,e.to)
    val newEdges = edges + e
    val newNexts = nexts + (e.from->(nexts.getOrElse(e.from,Set.empty)+e))
    val newPrevs = prevs + (e.to->(prevs.getOrElse(e.to,Set.empty)+e))
    builder(newNodes,newEdges,newNexts,newPrevs)
  }
  def addNode(n: Node) : Graph = {
    val newNodes = nodes + n
    builder(newNodes,edges,nexts,prevs)
  }
  def getNexts(n: Node) : Set[Node] = {
    nexts.get(n).map{_.aggregate(Set.empty[Node])(_+_.to,_++_)}.getOrElse(Set.empty[Node])
  }
  def getPrevs(n: Node) : Set[Node] = {
    prevs.get(n).map{_.aggregate(Set.empty[Node])(_+_.from,_++_)}.getOrElse(Set.empty[Node])
  }

  def builder(nodes: Set[Node], edges: Set[Edge], nexts: Map[Node,Set[Edge]], prevs: Map[Node,Set[Edge]]) : Graph
}
