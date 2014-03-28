package com.simplytyped.yoyak.graph

import org.scalatest.{Matchers, FunSuite}
import com.simplytyped.yoyak.graph.GraphGenerator.{IntEdge, IntegerImmutableGraph, IntNode}
import com.simplytyped.yoyak.graph.algo.GraphRefactoringImpl

/**
 * Created by ihji on 3/27/14.
 */
class GraphRefactoringTest extends FunSuite with Matchers {
  test("merge singly paired nodes") {
    val graph : IntegerImmutableGraph =
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
    val resultGraph : IntegerImmutableGraph =
      """digraph yoyak {
        |  1 -> 5;
        |  1 -> 6;
        |  5 -> 7;
        |  6 -> 7;
        |  7 -> 8;
        |  8 -> 8;
        |}
      """.stripMargin
    val merger = (x: IntNode, y: IntNode) => if(x.data < y.data) x else y
    val refactor = new GraphRefactoringImpl[IntNode,IntEdge,IntegerImmutableGraph]
    val refactoredGraph = refactor.mergePairedNodes(merger)(graph)
    refactoredGraph should be (resultGraph)
  }
}
