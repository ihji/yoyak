package com.simplytyped.yoyak.analysis

import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.Galois.{SetAbstraction, GaloisIdentity}
import com.simplytyped.yoyak.framework.domain.{ArithmeticOps, LatticeOps}
import com.simplytyped.yoyak.framework.domain.arith.IntervalInt
import com.simplytyped.yoyak.framework.domain.mem.MemDom
import com.simplytyped.yoyak.framework.semantics.{StdSemantics, AbstractTransferable}
import com.simplytyped.yoyak.il.cfg.CFG

class IntervalAnalysis(cfg: CFG) {
  def run() = {
    import IntervalAnalysis.{memDomOps,absTransfer}
    val analysis = new FlowSensitiveForwardAnalysis[GaloisIdentity[MemDom[IntervalInt,SetAbstraction[Any]]]](cfg)
    val output = analysis.compute
    output
  }
}

object IntervalAnalysis {
  implicit val absTransfer : AbstractTransferable[GaloisIdentity[MemDom[IntervalInt,SetAbstraction[Any]]]] = new StdSemantics[IntervalInt,SetAbstraction[Any],MemDom[IntervalInt,SetAbstraction[Any]]] {
    val arithOps: ArithmeticOps[IntervalInt] = IntervalInt.arithOps
  }

  implicit val memDomOps : LatticeOps[GaloisIdentity[MemDom[IntervalInt,SetAbstraction[Any]]]] = MemDom.ops[IntervalInt,SetAbstraction[Any]]
}
