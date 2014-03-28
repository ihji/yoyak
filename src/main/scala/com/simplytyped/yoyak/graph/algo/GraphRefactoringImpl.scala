package com.simplytyped.yoyak.graph.algo

import com.simplytyped.yoyak.graph.{NodeLike, EdgeLike, GraphLike}

class GraphRefactoringImpl[Node <: NodeLike, Edge <: EdgeLike[Node], Graph <: GraphLike[Node,Edge,Graph]] extends GraphRefactoring[Graph,Node] {
  def mergePairedNodes(merger: (Node,Node)=>Node)(g: Graph) : Graph = {
    def findMerged(remapper: Map[Node,Node])(n: Node) : Node = {
      val resultOpt = remapper.get(n)
      if(resultOpt.isEmpty) n
      else if(resultOpt.get == n) n
      else findMerged(remapper)(resultOpt.get)
    }
    // find the edges which have a single next node
    val oneNexts = g.nexts.filter{_._2.size == 1}.flatMap{_._2}
    // and find the subset of the edges which also have a single prev node
    val pairedEdges = oneNexts.filter{x=>g.getPrevs(x.to).size == 1}
    val (_,finalG) = pairedEdges.foldLeft(Map.empty[Node,Node],g) {
      case ((remapper,g),e) =>
        val to = findMerged(remapper)(e.to)
        val from = findMerged(remapper)(e.from)
        val newNode = merger(from,to)
        val newRemapper = remapper+(to->newNode)+(from->newNode)
        val outgoingNodesOfTwo = g.getNexts(to).map{findMerged(newRemapper)}
        val incomingNodesOfTwo = g.getPrevs(from).map{findMerged(newRemapper)}
        val vanillaG = g.removeNode(to).removeNode(from)
        val incomingAdded = incomingNodesOfTwo.foldLeft(vanillaG){_.addEdge(_,newNode)}
        val newG = outgoingNodesOfTwo.foldLeft(incomingAdded){_.addEdge(newNode,_)}
        (newRemapper,newG)
    }
    finalG
  }
}
