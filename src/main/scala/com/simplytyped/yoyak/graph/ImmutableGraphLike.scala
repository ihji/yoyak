package com.simplytyped.yoyak.graph

/**
 * trait for immutable graphs
 */
trait ImmutableGraphLike[Node <: NodeLike[Node], Edge <: EdgeLike[Node], Graph <: GraphLike[Node,Edge,Graph]] extends GraphLike[Node,Edge,Graph] { self: Graph =>
  def addEdge(from: Node, to: Node) : Graph = {
    val newE = newEdge(from, to)
    addEdge(newE)
  }
  def addEdge(e: Edge) : Graph = {
    val newNodes = nodes ++ Seq(e.from,e.to)
    val newEdges = edges + e
    val newNexts = nexts + (e.from->(nexts.getOrElse(e.from,Set.empty)+e))
    val newPrevs = prevs + (e.to->(prevs.getOrElse(e.to,Set.empty)+e))
    builder(newNodes,newEdges,newNexts,newPrevs)
  }
  def removeEdge(from: Node, to: Node) : Graph = {
    val newE = newEdge(from, to)
    removeEdge(newE)
  }
  def removeEdge(e: Edge) : Graph = {
    val newEdges = edges - e
    val newNextSet = nexts(e.from) - e
    val newNexts = if(newNextSet.isEmpty) nexts - e.from else nexts + (e.from->newNextSet)
    val newPrevSet = prevs(e.to) - e
    val newPrevs = if(newPrevSet.isEmpty) prevs - e.to else prevs + (e.to->newPrevSet)
    builder(nodes,newEdges,newNexts,newPrevs)
  }
  def removeNode(n: Node) : Graph = {
    val incomingEdges = prevs.getOrElse(n,Set.empty[Edge])
    val outgoingEdges = nexts.getOrElse(n,Set.empty[Edge])
    (incomingEdges ++ outgoingEdges).foldLeft(builder(nodes - n,edges,nexts,prevs)){_.removeEdge(_)}
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
