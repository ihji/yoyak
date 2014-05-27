package com.simplytyped.yoyak.graph.algo

import org.scalatest.{Matchers, FunSuite}
import com.simplytyped.yoyak.graph.GraphGenerator.{IntEdge, IntNode, IntegerImmutableGraph}

class GraphTraverseTest  extends FunSuite with Matchers {
  test("merge singly paired nodes") {
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
    val traverser = new GraphTraverseImpl[IntNode,IntEdge,IntegerImmutableGraph]
    val traverseStream = traverser.depthFirstTraverse(graph)
    traverseStream.toList.map{_.data} should be (List(1,2,3,4,5,7,8,9,10,6))
  }
}
