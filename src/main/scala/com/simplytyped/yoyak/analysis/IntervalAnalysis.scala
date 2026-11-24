package com.simplytyped.yoyak.analysis

import com.simplytyped.yoyak.analysis.IntervalAnalysis.Memory
import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.Galois.SetAbstraction
import com.simplytyped.yoyak.framework.domain.{ArithmeticOps, LatticeOps}
import com.simplytyped.yoyak.framework.domain.arith.{Interval, IntervalInt}
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
    val analysis = new FlowSensitiveForwardAnalysis[Memory](cfg)
    val output   = analysis.compute
    output
  }
}

object IntervalAnalysis {
  import IntervalInt.{arithOps, widening => intervalWidening}
  import SetAbstraction.boxedOps
  type Memory = MemDom[Interval, SetAbstraction[Any]]

  implicit val absTransfer: AbstractTransferable[Memory] =
    new StdSemantics[Interval, SetAbstraction[Any], Memory] {
      val arithOps: ArithmeticOps[Interval] = IntervalInt.arithOps
    }

  implicit val memDomOps: LatticeOps[Memory] =
    MemDom.ops[Interval, SetAbstraction[Any]]
  implicit val widening: Option[Widening[Memory]] = {
    implicit val NoWideningForSetAbstraction =
      Widening.NoWidening[SetAbstraction[Any]]
    Some(MemDom.widening[Interval, SetAbstraction[Any]])
  }
}
