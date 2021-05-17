package com.simplytyped.yoyak.il.opt.analysis

import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.Galois.{
  GaloisIdentity,
  SetAbstraction
}
import com.simplytyped.yoyak.framework.domain.{Galois, LatticeOps, MapDom}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable.Context
import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Value.Local
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}
import com.simplytyped.yoyak.il.opt.analysis.DefReachability.SetCoreStmt

class DefReachability {
  def run(
      cfg: CFG
  ): MapDom[BasicBlock, GaloisIdentity[MapDom[Local, SetCoreStmt]]] = {
    import com.simplytyped.yoyak.il.opt.analysis.DefReachability._
    val analysis = new FlowSensitiveForwardAnalysis[
      GaloisIdentity[MapDom[Local, SetCoreStmt]]
    ](cfg)
    val output = analysis.compute
    output
  }
}

object DefReachability {
  type SetCoreStmt = SetAbstraction[CoreStmt]

  implicit val ops: LatticeOps[GaloisIdentity[MapDom[Local, SetCoreStmt]]] =
    MapDom.ops[Local, SetCoreStmt]
  implicit val absTransfer
      : AbstractTransferable[GaloisIdentity[MapDom[Local, SetCoreStmt]]] =
    new AbstractTransferable[GaloisIdentity[MapDom[Local, SetCoreStmt]]] {

      override protected def transferInvoke(
          stmt: Invoke,
          input: MapDom[Local, SetCoreStmt]
      )(implicit context: Context): MapDom[Local, SetCoreStmt] = {
        if (stmt.ret.nonEmpty) input.update(stmt.ret.get -> Set(stmt))
        else input
      }

      override protected def transferIdentity(
          stmt: Identity,
          input: MapDom[Local, SetCoreStmt]
      )(implicit context: Context): MapDom[Local, SetCoreStmt] =
        input.update(stmt.lv -> Set(stmt))

      override protected def transferAssign(
          stmt: Assign,
          input: MapDom[Local, SetCoreStmt]
      )(implicit context: Context): MapDom[Local, SetCoreStmt] = {
        if (stmt.lv.isInstanceOf[Local])
          input.update(stmt.lv.asInstanceOf[Local] -> Set(stmt))
        else input
      }
    }
}
