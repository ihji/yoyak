package com.simplytyped.yoyak.framework

import com.simplytyped.yoyak.framework.algo.{
  WideningAtLoopHeads,
  FlowSensitiveFixedPointComputation,
  FlowInsensitiveFixedPointComputation
}
import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain.{Galois, MapDom, LatticeOps}
import com.simplytyped.yoyak.framework.semantics.{
  Widening,
  AbstractTransferable
}
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

object BackwardAnalysis {

  // FIXME: we should also take a reverse order while analyzing statements in a single block
  class FlowInsensitiveBackwardAnalysis[D <: Galois](val cfg: CFG)(implicit
      val ops: LatticeOps[D],
      val absTransfer: AbstractTransferable[D],
      val widening: Option[Widening[D]] = None
  ) extends FlowInsensitiveFixedPointComputation[D]
      with WideningAtLoopHeads[D] {
    def getNextBlocks(bb: BasicBlock) = cfg.getPrevs(bb).toList
    def compute(input: D#Abst): D#Abst = {
      val startNodes = cfg.getExit.toList.flatMap { getNextBlocks }
      computeFixedPoint(input, startNodes)
    }
  }

  class FlowSensitiveBackwardAnalysis[D <: Galois](val cfg: CFG)(implicit
      val ops: LatticeOps[D],
      val absTransfer: AbstractTransferable[D],
      val widening: Option[Widening[D]] = None
  ) extends FlowSensitiveFixedPointComputation[D]
      with WideningAtLoopHeads[D] {
    def getNextBlocks(bb: BasicBlock) = cfg.getPrevs(bb).toList
    def memoryFetcher(map: MapDom[BasicBlock, D], b: BasicBlock) =
      cfg.getNexts(b).toList.map { map.get }
    def compute: MapDom[BasicBlock, D] = {
      val startNodes = cfg.getExit.toList.flatMap { getNextBlocks }
      computeFixedPoint(startNodes)
    }
  }

}
