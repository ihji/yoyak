package com.simplytyped.yoyak.analysis

import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.Galois.{SetAbstraction, GaloisIdentity}
import com.simplytyped.yoyak.framework.domain.LatticeOps
import com.simplytyped.yoyak.framework.domain.arith.IntervalInt
import com.simplytyped.yoyak.framework.domain.mem.MemDom
import com.simplytyped.yoyak.framework.domain.mem.MemElems.{AbsTop, AbsArith, AbsValue}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable.Context
import com.simplytyped.yoyak.il.CommonIL.Statement.Assign
import com.simplytyped.yoyak.il.CommonIL.Value
import com.simplytyped.yoyak.il.CommonIL.Value.BinOp
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
  implicit val absTransfer : AbstractTransferable[GaloisIdentity[MemDom[IntervalInt,SetAbstraction[Any]]]] = new AbstractTransferable[GaloisIdentity[MemDom[IntervalInt,SetAbstraction[Any]]]] {
    override protected def transferAssign(stmt: Assign, input: MemDom[IntervalInt, SetAbstraction[Any]])(implicit context: Context): MemDom[IntervalInt, SetAbstraction[Any]] = {
      val value = eval(stmt.rv)
      input.update(stmt.lv->value) // FIXME: should implement complete object model (object allocation, field reference, etc.)
    }
  }
  def eval(v: Value.t) : AbsValue[IntervalInt,SetAbstraction[Any]] = {
    v match {
      case x : Value.Constant => AbsArith(IntervalInt.arithOps.lift(x))
      case Value.CompBinExp(lv,op,rv) =>
        val l = eval(lv)
        val r = eval(rv)
        if(l.isInstanceOf[AbsArith[IntervalInt]] && r.isInstanceOf[AbsArith[IntervalInt]]) {
          val linter = l.asInstanceOf[AbsArith[IntervalInt]].data
          val rinter = r.asInstanceOf[AbsArith[IntervalInt]].data
          op match {
            case BinOp.+ => AbsArith(IntervalInt.arithOps.+(linter,rinter))
            case BinOp.- => AbsArith(IntervalInt.arithOps.+(linter,rinter))
            case BinOp.* => AbsArith(IntervalInt.arithOps.+(linter,rinter))
            case BinOp./ => AbsArith(IntervalInt.arithOps.+(linter,rinter))
            case _ => AbsTop
          }
        } else AbsTop
      case _ => AbsTop
    }
  }
  implicit val memDomOps : LatticeOps[GaloisIdentity[MemDom[IntervalInt,SetAbstraction[Any]]]] = MemDom.ops[IntervalInt,SetAbstraction[Any]]
}
