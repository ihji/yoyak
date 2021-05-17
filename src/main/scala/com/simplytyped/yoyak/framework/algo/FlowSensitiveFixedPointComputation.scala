package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.{Galois, MapDom, LatticeOps}
import com.simplytyped.yoyak.framework.semantics.Widening
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait FlowSensitiveFixedPointComputation[D <: Galois]
    extends FlowSensitiveIteration[D]
    with CfgNavigator[D]
    with DoWidening[D] {
  implicit val ops: LatticeOps[D]

  val worklist = Worklist.empty[BasicBlock]

  private def getInput(
      map: MapDom[BasicBlock, D],
      inputs: Seq[D#Abst]
  ): D#Abst = {
    val input = inputs.foldLeft(ops.bottom) { (d, i) =>
      ops.\/(d, i)
    }
    input
  }

  def computeFixedPoint(
      startNodes: List[BasicBlock]
  )(implicit widening: Option[Widening[D]] = None): MapDom[BasicBlock, D] = {
    worklist.add(startNodes: _*)
    var map = MapDom.empty[BasicBlock, D]
    while (worklist.size() > 0) {
      val bb             = worklist.pop().get
      val prevInputs     = memoryFetcher(map, bb)
      val prev           = getInput(map, prevInputs)
      val (mapOut, next) = work(map, prev, bb)
      val orig           = map.get(bb)
      val isStable       = ops.partialCompare(next, orig)
      if (isStable.isNaN) {
        // XXX: abstract transfer function is not distributive. should report this error.
        println("error: abs. transfer func. is not distributive")
      }
      if (isStable > 0) {
        val widened = if (widening.nonEmpty) {
          doWidening(widening.get)(orig, next, bb)
        } else next
        map = mapOut.update(bb -> widened)
        val nextWork = getNextBlocks(bb)
        worklist.add(nextWork: _*)
      }
    }
    map
  }
}
