package com.simplytyped.yoyak.graph

import scala.util.parsing.combinator.JavaTokenParsers
import com.simplytyped.yoyak.graph.GraphGenerator.{
  IntEdge,
  IntNode,
  IntegerImmutableGraph
}

class IntegerGraphParser extends JavaTokenParsers {
  def graph: Parser[IntegerImmutableGraph] =
    "digraph yoyak {" ~> rep(node) ~ rep(edge) <~ "}" ^^ { case nodes ~ edges =>
      val graph     = IntegerImmutableGraph.empty
      val nodeAdded = nodes.foldLeft(graph) { _.addNode(_) }
      edges.foldLeft(nodeAdded) { _.addEdge(_) }
    }
  def node: Parser[IntNode] =
    decimalNumber <~ ";" ^^ { x => IntNode(x.toInt) }
  def edge: Parser[IntEdge] =
    decimalNumber ~ "->" ~ decimalNumber ~ ";" ^^ { case from ~ _ ~ to ~ _ =>
      IntEdge(IntNode(from.toInt), IntNode(to.toInt))
    }
}
