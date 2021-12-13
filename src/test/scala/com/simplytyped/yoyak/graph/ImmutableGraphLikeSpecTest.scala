package com.simplytyped.yoyak.graph

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop._
import com.simplytyped.yoyak.graph.GraphGenerator.{
  IntEdge,
  IntNode,
  IntegerImmutableGraph
}

/** Graph build test
  */
class ImmutableGraphLikeSpecTest extends AnyFunSuite with Matchers with Checkers {
  test("add one edge") {
    check {
      forAll(GraphGenerator.graphGen) { (g: IntegerImmutableGraph) =>
        val edge = IntEdge(IntNode(101), IntNode(102))
        g.nodes.size == (g.addEdge(edge).nodes.size - 2)
      }
    }
    check {
      forAll(GraphGenerator.graphGen) { (g: IntegerImmutableGraph) =>
        val edge = IntEdge(IntNode(101), IntNode(102))
        g.edges.size == (g.addEdge(edge).edges.size - 1)
      }
    }
  }
  test("should ignore addition of a duplicated edge") {
    check {
      forAll(GraphGenerator.graphGen) { (g: IntegerImmutableGraph) =>
        val duplicatedEdge = g.edges.head
        g.nodes.size == g.addEdge(duplicatedEdge).nodes.size
      }
    }
    check {
      forAll(GraphGenerator.graphGen) { (g: IntegerImmutableGraph) =>
        val duplicatedEdge = g.edges.head
        g.edges.size == g.addEdge(duplicatedEdge).edges.size
      }
    }
  }
  test("replaceNode()") {
    val graph: IntegerImmutableGraph =
      """digraph yoyak {
      |  1;
      |  2;
      |  3;
      |  1 -> 2;
      |  2 -> 3;
      |  3 -> 1;
      |}
    """.stripMargin
    val newGraph = graph.replaceNode(IntNode(2), IntNode(4))
    newGraph.nodes should be(Set(IntNode(1), IntNode(3), IntNode(4)))
    newGraph.edges should be(
      Set(
        IntEdge(IntNode(1), IntNode(4)),
        IntEdge(IntNode(4), IntNode(3)),
        IntEdge(IntNode(3), IntNode(1))
      )
    )
  }
}
