package com.simplytyped.yoyak.framework

import com.simplytyped.yoyak.framework.algo.{FlowSensitiveWorklistIteration, FlowInsensitiveWorklistIteration}
import com.simplytyped.yoyak.framework.domain.{MapDom, LatticeOps}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

object ForwardAnalysis {

  class FlowInsensitiveForwardAnalysis[D](cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowInsensitiveWorklistIteration[D] {
    def compute(input: D): D = {
      val startNodes = cfg.nodes.filter { n => cfg.getPrevs(n).isEmpty}.toList
      val getNextBlocks = { bb: BasicBlock => cfg.getNexts(bb).toList}
      computeFixedPoint(input, startNodes, getNextBlocks)
    }
  }

  class FlowSensitiveForwardAnalysis[D](cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowSensitiveWorklistIteration[D] {
    implicit val mapDomOps: LatticeOps[MapDom[BasicBlock,D]] = MapDom.ops[BasicBlock,D]
    def compute : MapDom[BasicBlock,D] = {
      val startNodes = cfg.nodes.filter { n => cfg.getPrevs(n).isEmpty}.toList
      val getPrevBlocks = { bb: BasicBlock => cfg.getPrevs(bb).toList}
      val getNextBlocks = { bb: BasicBlock => cfg.getNexts(bb).toList}
      computeFixedPoint(startNodes, getPrevBlocks, getNextBlocks)
    }
  }
}