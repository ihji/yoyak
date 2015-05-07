package com.simplytyped.yoyak.il.opt.analysis

import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain.{Galois, LatticeOps, MapDom}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Value.Local
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}
import com.simplytyped.yoyak.il.opt.analysis.DefReachability.SetCoreStmt

class DefReachability {
  def run(cfg: CFG) : MapDom[BasicBlock,GaloisIdentity[MapDom[Local,SetCoreStmt]]] = {
    import com.simplytyped.yoyak.il.opt.analysis.DefReachability._
    val analysis = new FlowSensitiveForwardAnalysis[GaloisIdentity[MapDom[Local,SetCoreStmt]]](cfg)
    val output = analysis.compute
    output
  }
}

object DefReachability {
  class SetCoreStmt extends Galois {
    override type Conc = CoreStmt
    override type Abst = Set[CoreStmt]
  }
  implicit val valueOps : LatticeOps[SetCoreStmt] = new LatticeOps[SetCoreStmt] {
    override def \/(lhs: Set[CoreStmt], rhs: Set[CoreStmt]): Set[CoreStmt] = lhs ++ rhs

    override val bottom: Set[CoreStmt] = Set.empty[CoreStmt]

    override def <=(lhs: Set[CoreStmt], rhs: Set[CoreStmt]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None
  }
  implicit val ops : LatticeOps[GaloisIdentity[MapDom[Local,SetCoreStmt]]] = MapDom.ops[Local,SetCoreStmt]
  implicit val absTransfer : AbstractTransferable[GaloisIdentity[MapDom[Local,SetCoreStmt]]] = new AbstractTransferable[GaloisIdentity[MapDom[Local, SetCoreStmt]]] {

    override protected def transferInvoke(stmt: Invoke, input: MapDom[Local, SetCoreStmt]): MapDom[Local, SetCoreStmt] = {
      if(stmt.ret.nonEmpty) input.update(stmt.ret.get -> Set(stmt))
      else input
    }

    override protected def transferIdentity(stmt: Identity, input: MapDom[Local, SetCoreStmt]): MapDom[Local, SetCoreStmt] = input.update(stmt.lv -> Set(stmt))

    override protected def transferAssign(stmt: Assign, input: MapDom[Local, SetCoreStmt]): MapDom[Local, SetCoreStmt] = {
      if(stmt.lv.isInstanceOf[Local]) input.update(stmt.lv.asInstanceOf[Local] -> Set(stmt))
      else input
    }
  }
}