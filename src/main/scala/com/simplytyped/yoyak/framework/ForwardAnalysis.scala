package com.simplytyped.yoyak.framework

import com.simplytyped.yoyak.framework.algo.{WideningAtLoopHeads, FlowSensitiveFixedPointComputation, FlowInsensitiveFixedPointComputation}
import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain.{Galois, MapDom, LatticeOps}
import com.simplytyped.yoyak.framework.semantics.{Widening, AbstractTransferable}
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

object ForwardAnalysis {

  class FlowInsensitiveForwardAnalysis[D<:Galois](val cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D], val widening: Option[Widening[D]] = None) extends FlowInsensitiveFixedPointComputation[D] with WideningAtLoopHeads[D] {
    def getNextBlocks(bb: BasicBlock) = cfg.getNexts(bb).toList
    def compute(input: D#Abst): D#Abst = {
      val startNodes = cfg.getEntry.toList.flatMap{getNextBlocks}
      computeFixedPoint(input, startNodes)
    }
  }

  class FlowSensitiveForwardAnalysis[D<:Galois](val cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D], val widening: Option[Widening[D]] = None) extends FlowSensitiveFixedPointComputation[D] with WideningAtLoopHeads[D] {
    def getNextBlocks(bb: BasicBlock) = cfg.getNexts(bb).toList
    def memoryFetcher(map: MapDom[BasicBlock,D], b: BasicBlock) = cfg.getPrevs(b).toList.map{map.get}
    def compute : MapDom[BasicBlock,D] = {
      val startNodes = cfg.getEntry.toList.flatMap{getNextBlocks}
      computeFixedPoint(startNodes)
    }
  }
}