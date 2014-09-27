package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.{MapDom, LatticeOps}
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait FlowSensitiveWorklistIteration[D] extends Iteration[D] {
  implicit val ops : LatticeOps[D]
  implicit val mapDomOps : LatticeOps[MapDom[BasicBlock,D]]

  val worklist = Worklist.empty[BasicBlock]

  private def getInput(map: MapDom[BasicBlock,D], prevBlocks: Seq[BasicBlock]) : D = {
    val input = prevBlocks.foldLeft(ops.bottom) {
      (d,b) => ops.\/(d,map.get(b))
    }
    input
  }

  def computeFixedPoint(startNodes: List[BasicBlock], getPrevBlocks: BasicBlock => Seq[BasicBlock], getNextBlocks: BasicBlock => Seq[BasicBlock]) : MapDom[BasicBlock,D] = {
    assert(startNodes.size > 0)
    worklist.add(startNodes:_*)
    var map = MapDom.empty[BasicBlock,D]
    while(worklist.size() > 0) {
      val bb = worklist.pop().get
      val prevBlocks = getPrevBlocks(bb)
      val prev = getInput(map,prevBlocks)
      val next = work(prev,bb)
      val nextMap = map.update(bb->next)
      if(!mapDomOps.<=(nextMap,map)) {
        val nextWork = getNextBlocks(bb)
        worklist.add(nextWork:_*)
        map = nextMap
      }
    }
    map
  }
}
