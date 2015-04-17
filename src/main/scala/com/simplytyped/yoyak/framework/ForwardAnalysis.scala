package com.simplytyped.yoyak.framework

import com.simplytyped.yoyak.framework.algo.{FlowSensitiveFixedPointComputation, FlowInsensitiveFixedPointComputation}
import com.simplytyped.yoyak.framework.domain.{MapDom, LatticeOps}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

object ForwardAnalysis {

  class FlowInsensitiveForwardAnalysis[D](var cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowInsensitiveFixedPointComputation[D] {
    def getNextBlocks(bb: BasicBlock) = cfg.getNexts(bb).toList
    def compute(input: D): D = {
      val startNodes = cfg.getEntry.toList
      computeFixedPoint(input, startNodes)
    }
  }

  class FlowSensitiveForwardAnalysis[D](var cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowSensitiveFixedPointComputation[D] {
    implicit val mapDomOps: LatticeOps[MapDom[BasicBlock,D]] = MapDom.ops[BasicBlock,D]
    def getPrevBlocks(bb: BasicBlock) = cfg.getPrevs(bb).toList
    def getNextBlocks(bb: BasicBlock) = cfg.getNexts(bb).toList
    def memoryFetcher(map: MapDom[BasicBlock,D], b: BasicBlock) = map.get(b)
    def compute : MapDom[BasicBlock,D] = {
      val startNodes = cfg.getEntry.toList
      computeFixedPoint(startNodes)
    }
  }
}