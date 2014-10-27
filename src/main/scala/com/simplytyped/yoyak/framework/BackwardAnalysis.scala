package com.simplytyped.yoyak.framework

import com.simplytyped.yoyak.framework.algo.{FlowSensitiveWorklistIteration, FlowInsensitiveWorklistIteration}
import com.simplytyped.yoyak.framework.domain.{MapDom, LatticeOps}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

object BackwardAnalysis {

  class FlowInsensitiveBackwardAnalysis[D](cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowInsensitiveWorklistIteration[D] {
    def compute(input: D): D = {
      val startNodes = cfg.getExit.toList
      val getNextBlocks = { bb: BasicBlock => cfg.getPrevs(bb).toList}
      computeFixedPoint(input, startNodes, getNextBlocks)
    }
  }

  class FlowSensitiveBackwardAnalysis[D](cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowSensitiveWorklistIteration[D] {
    implicit val mapDomOps: LatticeOps[MapDom[BasicBlock,D]] = MapDom.ops[BasicBlock,D]
    def compute : MapDom[BasicBlock,D] = {
      val startNodes = cfg.getExit.toList
      val getPrevBlocks = { bb: BasicBlock => cfg.getNexts(bb).toList}
      val getNextBlocks = { bb: BasicBlock => cfg.getPrevs(bb).toList}
      computeFixedPoint(startNodes,getPrevBlocks,getNextBlocks)
    }
  }

}