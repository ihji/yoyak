package com.simplytyped.yoyak.graph

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import com.simplytyped.yoyak.graph.GraphGenerator.{IntEdge, IntNode, IntegerImmutableGraph}

/**
 * Graph build test
 */
class ImmutableGraphLikeTest extends FunSuite with Matchers with Checkers {
  test("add one edge") {
    check {
      forAll(GraphGenerator.graphGen) {
        (g: IntegerImmutableGraph) =>
          val edge = IntEdge(IntNode(101), IntNode(102))
          g.nodes.size == (g.addEdge(edge).nodes.size - 2)
      }
    }
    check {
      forAll(GraphGenerator.graphGen) {
        (g: IntegerImmutableGraph) =>
          val edge = IntEdge(IntNode(101), IntNode(102))
          g.edges.size == (g.addEdge(edge).edges.size - 1)
      }
    }
  }
  test("should ignore addition of a duplicated edge") {
    check {
      forAll(GraphGenerator.graphGen) {
        (g: IntegerImmutableGraph) =>
          val duplicatedEdge = g.edges.head
          g.nodes.size == g.addEdge(duplicatedEdge).nodes.size
      }
    }
    check {
      forAll(GraphGenerator.graphGen) {
        (g: IntegerImmutableGraph) =>
          val duplicatedEdge = g.edges.head
          g.edges.size == g.addEdge(duplicatedEdge).edges.size
      }
    }
  }
}
