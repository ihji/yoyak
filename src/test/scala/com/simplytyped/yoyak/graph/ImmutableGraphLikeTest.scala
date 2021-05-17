package com.simplytyped.yoyak.graph

import org.scalatest.{Matchers, FunSuite}
import com.simplytyped.yoyak.graph.GraphGenerator.IntegerImmutableGraph

/** Created by ihji on 3/24/14.
  */
class ImmutableGraphLikeTest extends FunSuite with Matchers {
  test("basic graph construction") {
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

    graph.nodes.size should be(3)
    graph.edges.size should be(3)
  }
}
