package com.simplytyped.yoyak.graph

/**
 * very basic graph functionalities
 */
trait GraphLike[Node <: NodeLike, Edge <: EdgeLike[Node], Graph <: GraphLike[Node,Edge,Graph]] { self: Graph =>
  val edges : Set[Edge]
  val nodes : Set[Node]

  def addEdge(from: Node, to: Node) : Graph
  def addEdge(e: Edge) : Graph
  def addNode(n: Node) : Graph
  def newEdge(from: Node, to: Node) : Edge
  def getNexts(n: Node) : Set[Node]
  def getPrevs(n: Node) : Set[Node]

  def countNodes : Int = nodes.size
  def countEdges : Int = edges.size

  def ++(other: Graph) : Graph = {
    val nodeAdded = other.nodes.foldLeft(self){_.addNode(_)}
    other.edges.foldLeft(nodeAdded){_.addEdge(_)}
  }

  override def toString : String = {
    val buf = new StringBuffer()
    buf.append("digraph yoyak {\n")
    buf.append(nodes.mkString(";\n"))
    buf.append(";\n")
    buf.append(edges.mkString(";\n"))
    buf.append(";\n}")
    buf.toString
  }
}
