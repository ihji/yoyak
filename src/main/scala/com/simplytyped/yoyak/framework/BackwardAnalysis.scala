package com.simplytyped.yoyak.framework

import com.simplytyped.yoyak.framework.algo.{FlowSensitiveFixedPointComputation, FlowInsensitiveFixedPointComputation}
import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain.{Galois, MapDom, LatticeOps}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

object BackwardAnalysis {

  class FlowInsensitiveBackwardAnalysis[D<:Galois](var cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowInsensitiveFixedPointComputation[D] {
    def getNextBlocks(bb: BasicBlock) = cfg.getPrevs(bb).toList
    def compute(input: D#Abst): D#Abst = {
      val startNodes = cfg.getExit.toList
      computeFixedPoint(input, startNodes)
    }
  }

  class FlowSensitiveBackwardAnalysis[D <: Galois](var cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowSensitiveFixedPointComputation[D] {
    implicit val mapDomOps: LatticeOps[GaloisIdentity[MapDom[BasicBlock,D]]] = MapDom.ops[BasicBlock,D]
    def getNextBlocks(bb: BasicBlock) = cfg.getPrevs(bb).toList
    def memoryFetcher(map: MapDom[BasicBlock,D], b: BasicBlock) = cfg.getNexts(b).toList.map{map.get}
    def compute : MapDom[BasicBlock,D] = {
      val startNodes = cfg.getExit.toList
      computeFixedPoint(startNodes)
    }
  }

}