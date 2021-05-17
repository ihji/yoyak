package com.simplytyped.yoyak.analysis

import com.simplytyped.yoyak.analysis.IntervalAnalysis.GMemory
import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.Galois.{
  SetAbstraction,
  GaloisIdentity
}
import com.simplytyped.yoyak.framework.domain.{ArithmeticOps, LatticeOps}
import com.simplytyped.yoyak.framework.domain.arith.IntervalInt
import com.simplytyped.yoyak.framework.domain.mem.MemDom
import com.simplytyped.yoyak.framework.semantics.{
  Widening,
  StdSemantics,
  AbstractTransferable
}
import com.simplytyped.yoyak.il.cfg.CFG

class IntervalAnalysis(cfg: CFG) {
  def run() = {
    import IntervalAnalysis.{memDomOps, absTransfer, widening}
    val analysis = new FlowSensitiveForwardAnalysis[GMemory](cfg)
    val output   = analysis.compute
    output
  }
}

object IntervalAnalysis {
  type Memory  = MemDom[IntervalInt, SetAbstraction[Any]]
  type GMemory = GaloisIdentity[Memory]
  implicit val absTransfer: AbstractTransferable[GMemory] =
    new StdSemantics[IntervalInt, SetAbstraction[Any], Memory] {
      val arithOps: ArithmeticOps[IntervalInt] = IntervalInt.arithOps
    }

  implicit val memDomOps: LatticeOps[GMemory] =
    MemDom.ops[IntervalInt, SetAbstraction[Any]]
  implicit val widening: Option[Widening[GMemory]] = {
    implicit val NoWideningForSetAbstraction =
      Widening.NoWidening[SetAbstraction[Any]]
    Some(MemDom.widening[IntervalInt, SetAbstraction[Any]])
  }
}
