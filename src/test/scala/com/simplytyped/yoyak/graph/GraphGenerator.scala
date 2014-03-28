package com.simplytyped.yoyak.graph

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

/**
 * automatic test graph generator
 */
object GraphGenerator {
  case class IntNode(data: Int) extends NodeLike { type D = Int }
  case class IntEdge(from: IntNode, to: IntNode) extends EdgeLike[IntNode] { type L = Option[Nothing]; val label = None }
  case class IntegerImmutableGraph(nodes: Set[IntNode], edges: Set[IntEdge], nexts: Map[IntNode,Set[IntEdge]], prevs: Map[IntNode,Set[IntEdge]]) extends ImmutableGraphLike[IntNode,IntEdge,IntegerImmutableGraph] {

    def newEdge(from: IntNode, to: IntNode): IntEdge = IntEdge(from,to)

    def builder(nodes: Set[IntNode], edges: Set[IntEdge], nexts: Map[IntNode, Set[IntEdge]], prevs: Map[IntNode, Set[IntEdge]]): IntegerImmutableGraph = {
      IntegerImmutableGraph(nodes,edges,nexts,prevs)
    }
  }
  object IntegerImmutableGraph {
    val empty = IntegerImmutableGraph(Set.empty,Set.empty,Map.empty,Map.empty)
    implicit def str2graph(s: String) : IntegerImmutableGraph = {
      val parser = new IntegerGraphParser
      parser.parseAll(parser.graph,s).get
    }
  }

  val singleGraphGen : Gen[IntegerImmutableGraph] = for {
    from <- Gen.choose(1,100)
    to <- Gen.choose(1,100)
  } yield {
    IntegerImmutableGraph.empty.addEdge(IntNode(from),IntNode(to))
  }

  def joinGraphGen(count: Int) : Gen[IntegerImmutableGraph] = for {
    _ <- arbitrary[Int]
    first <- graphGenAux(count)
    second <- graphGenAux(count)
    firstIndex <- Gen.choose(0,first.nodes.size-1)
    secondIndex <- Gen.choose(0,second.nodes.size-1)
  } yield {
    (first ++ second).addEdge(first.nodes.toList(firstIndex),second.nodes.toList(secondIndex))
  }
  def graphGenAux(count : Int = 0) : Gen[IntegerImmutableGraph] =
    if(count < 5) joinGraphGen(count+1)
    else if(count > 50) singleGraphGen
    else Gen.oneOf(singleGraphGen,joinGraphGen(count+1))
  def graphGen = graphGenAux()
}
