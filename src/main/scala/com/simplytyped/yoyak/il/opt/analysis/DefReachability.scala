package com.simplytyped.yoyak.il.opt.analysis

import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.{LatticeOps, MapDom}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Value.Local
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

class DefReachability {
  def run(cfg: CFG) : MapDom[BasicBlock,MapDom[Local,Set[CoreStmt]]] = {
    import com.simplytyped.yoyak.il.opt.analysis.DefReachability._
    val analysis = new FlowSensitiveForwardAnalysis[MapDom[Local,Set[CoreStmt]]](cfg)
    val output = analysis.compute
    output
  }
}

object DefReachability {
  implicit val valueOps : LatticeOps[Set[CoreStmt]] = new LatticeOps[Set[CoreStmt]] {
    override def \/(lhs: Set[CoreStmt], rhs: Set[CoreStmt]): Set[CoreStmt] = lhs ++ rhs

    override val bottom: Set[CoreStmt] = Set.empty[CoreStmt]

    override def <=(lhs: Set[CoreStmt], rhs: Set[CoreStmt]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None
  }
  implicit val ops : LatticeOps[MapDom[Local,Set[CoreStmt]]] = MapDom.ops[Local,Set[CoreStmt]]
  implicit val absTransfer : AbstractTransferable[MapDom[Local,Set[CoreStmt]]] = new AbstractTransferable[MapDom[Local, Set[CoreStmt]]] {

    override protected def transferInvoke(stmt: Invoke, input: MapDom[Local, Set[CoreStmt]]): MapDom[Local, Set[CoreStmt]] = {
      if(stmt.ret.nonEmpty) input.update(stmt.ret.get -> Set(stmt))
      else input
    }

    override protected def transferIdentity(stmt: Identity, input: MapDom[Local, Set[CoreStmt]]): MapDom[Local, Set[CoreStmt]] = input.update(stmt.lv -> Set(stmt))

    override protected def transferAssign(stmt: Assign, input: MapDom[Local, Set[CoreStmt]]): MapDom[Local, Set[CoreStmt]] = {
      if(stmt.lv.isInstanceOf[Local]) input.update(stmt.lv.asInstanceOf[Local] -> Set(stmt))
      else input
    }
  }
}