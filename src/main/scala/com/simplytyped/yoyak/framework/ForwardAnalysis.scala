package com.simplytyped.yoyak.framework

import com.simplytyped.yoyak.framework.algo.WorklistIteration
import com.simplytyped.yoyak.framework.domain.LatticeOps
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

class ForwardAnalysis[D](cfg: CFG)(implicit val ops: LatticeOps[D], val absTransfer: AbstractTransferable[D]) extends WorklistIteration[D] {
  def compute(input: D) : D = {
    val startNodes = cfg.nodes.filter{n => cfg.getPrevs(n).isEmpty}.toList
    val getNextWork = {bb : BasicBlock => cfg.getNexts(bb).toList}
    computeFixedPoint(input,startNodes,getNextWork)
  }
}
