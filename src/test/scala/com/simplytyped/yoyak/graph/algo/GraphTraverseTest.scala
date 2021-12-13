package com.simplytyped.yoyak.graph.algo

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import com.simplytyped.yoyak.graph.GraphGenerator.{
  IntEdge,
  IntNode,
  IntegerImmutableGraph
}

class GraphTraverseTest extends AnyFunSuite with Matchers {
  test("depth first traverse") {
    val graph: IntegerImmutableGraph =
      """digraph yoyak {
        |  1 -> 2;
        |  2 -> 3;
        |  3 -> 4;
        |  4 -> 5;
        |  4 -> 6;
        |  5 -> 7;
        |  6 -> 7;
        |  7 -> 8;
        |  8 -> 9;
        |  9 -> 10;
        |  10 -> 8;
        |}
      """.stripMargin
    val traverser =
      new GraphTraverseImpl[IntNode, IntEdge, IntegerImmutableGraph] {}
    val traverseStream = traverser.depthFirstTraverse(graph)
    traverseStream.toList.map { _.data } should be(
      List(1, 2, 3, 4, 5, 7, 8, 9, 10, 6)
    )
  }
  test("find loopheads: Simple") {
    val graph: IntegerImmutableGraph =
      """digraph yoyak {
        |  1 -> 2;
        |  2 -> 3;
        |  3 -> 4;
        |  4 -> 5;
        |  4 -> 6;
        |  5 -> 7;
        |  6 -> 7;
        |  7 -> 8;
        |  8 -> 9;
        |  9 -> 10;
        |  10 -> 8;
        |}
      """.stripMargin
    val traverser =
      new GraphTraverseImpl[IntNode, IntEdge, IntegerImmutableGraph] {}
    val loopHeads = traverser.findLoopheads(graph)
    loopHeads.map { _.data }.toSet should be(Set(8))
  }
  test("find loopheads: Nested") {
    val graph: IntegerImmutableGraph =
      """digraph yoyak {
        |  1 -> 2;
        |  2 -> 3;
        |  3 -> 4;
        |  4 -> 5;
        |  5 -> 6;
        |  6 -> 7;
        |  7 -> 8;
        |  8 -> 9;
        |  9 -> 10;
        |  9 -> 2;
        |  8 -> 3;
        |  7 -> 4;
        |}
      """.stripMargin
    val traverser =
      new GraphTraverseImpl[IntNode, IntEdge, IntegerImmutableGraph] {}
    val loopHeads = traverser.findLoopheads(graph)
    loopHeads.map { _.data }.toSet should be(Set(4, 3, 2))
  }
  test("find loopheads: Break") {
    val graph: IntegerImmutableGraph =
      """digraph yoyak {
        |  1 -> 2;
        |  2 -> 3;
        |  3 -> 4;
        |  4 -> 5;
        |  5 -> 6;
        |  6 -> 7;
        |  7 -> 8;
        |  8 -> 9;
        |  9 -> 10;
        |  9 -> 2;
        |  8 -> 3;
        |  4 -> 10;
        |}
      """.stripMargin
    val traverser =
      new GraphTraverseImpl[IntNode, IntEdge, IntegerImmutableGraph] {}
    val loopHeads = traverser.findLoopheads(graph)
    loopHeads.map { _.data }.toSet should be(Set(3, 2))
  }
  test("find loopheads: Loop in branch") {
    val graph: IntegerImmutableGraph =
      """digraph yoyak {
        |  1 -> 2;
        |  2 -> 3;
        |  3 -> 4;
        |  4 -> 5;
        |  5 -> 6;
        |  6 -> 7;
        |  4 -> 8;
        |  8 -> 9;
        |  9 -> 10;
        |  7 -> 11;
        |  10 -> 11;
        |  10 -> 8;
        |}
      """.stripMargin
    val traverser =
      new GraphTraverseImpl[IntNode, IntEdge, IntegerImmutableGraph] {}
    val loopHeads = traverser.findLoopheads(graph)
    loopHeads.map { _.data }.toSet should be(Set(8))
  }
}
