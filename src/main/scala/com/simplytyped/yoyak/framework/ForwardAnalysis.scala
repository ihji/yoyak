package com.simplytyped.yoyak.framework

import com.simplytyped.yoyak.framework.algo.{FlowSensitiveFixedPointComputation, FlowInsensitiveFixedPointComputation}
import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain.{Galois, MapDom, LatticeOps}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

object ForwardAnalysis {

  class FlowInsensitiveForwardAnalysis[D<:Galois](var cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowInsensitiveFixedPointComputation[D] {
    def getNextBlocks(bb: BasicBlock) = cfg.getNexts(bb).toList
    def compute(input: D#Abst): D#Abst = {
      val startNodes = cfg.getEntry.toList.flatMap{getNextBlocks}
      computeFixedPoint(input, startNodes)
    }
  }

  class FlowSensitiveForwardAnalysis[D<:Galois](var cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends FlowSensitiveFixedPointComputation[D] {
    implicit val mapDomOps: LatticeOps[GaloisIdentity[MapDom[BasicBlock,D]]] = MapDom.ops[BasicBlock,D]
    def getNextBlocks(bb: BasicBlock) = cfg.getNexts(bb).toList
    def memoryFetcher(map: MapDom[BasicBlock,D], b: BasicBlock) = cfg.getPrevs(b).toList.map{map.get}
    def compute : MapDom[BasicBlock,D] = {
      val startNodes = cfg.getEntry.toList.flatMap{getNextBlocks}
      computeFixedPoint(startNodes)
    }
  }
}