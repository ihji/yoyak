package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.{MapDom, LatticeOps}
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait FlowSensitiveFixedPointComputation[D] extends FlowSensitiveIteration[D] with CfgNavigator[D] {
  implicit val ops : LatticeOps[D]
  implicit val mapDomOps : LatticeOps[MapDom[BasicBlock,D]]

  val worklist = Worklist.empty[BasicBlock]

  private def getInput(map: MapDom[BasicBlock,D], inputs: Seq[D]) : D = {
    val input = inputs.foldLeft(ops.bottom) {
      (d,i) => ops.\/(d,i)
    }
    input
  }

  def computeFixedPoint(startNodes: List[BasicBlock]) : MapDom[BasicBlock,D] = {
    worklist.add(startNodes:_*)
    var map = MapDom.empty[BasicBlock,D]
    while(worklist.size() > 0) {
      val bb = worklist.pop().get
      val prevInputs = memoryFetcher(map,bb)
      val prev = getInput(map,prevInputs)
      val (mapOut,next) = work(map,prev,bb)
      val nextMap = mapOut.update(bb->next)
      val isStableOpt = mapDomOps.<=(nextMap,map)
      if(isStableOpt.isEmpty) println("error: abs. transfer func. is not distributive") // XXX: abstract transfer function is not distributive. should report this error.
      if(!isStableOpt.get) {
        val nextWork = getNextBlocks(bb)
        worklist.add(nextWork:_*)
        map = nextMap
      }
    }
    map
  }
}
