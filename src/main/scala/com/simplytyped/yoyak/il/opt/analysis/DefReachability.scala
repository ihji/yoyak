package com.simplytyped.yoyak.il.opt.analysis

import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.{LatticeOps, MapDom}
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Value.Local
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

class DefReachability {
  def run(cfg: CFG) : MapDom[BasicBlock,Map[Local,Set[CoreStmt]]] = {
    import com.simplytyped.yoyak.il.opt.analysis.DefReachability._
    val analysis = new FlowSensitiveForwardAnalysis[Map[Local,Set[CoreStmt]]](cfg)
    val output = analysis.compute
    output
  }
}

object DefReachability {
  implicit val ops : LatticeOps[Map[Local,Set[CoreStmt]]] = new LatticeOps[Map[Local,Set[CoreStmt]]] {
    override def <=(lhs: Map[Local, Set[CoreStmt]], rhs: Map[Local, Set[CoreStmt]]): Boolean = {
      lhs.forall{case (k,v) => v.subsetOf(rhs.getOrElse(k,Set.empty[CoreStmt]))}
    }

    override def \/(lhs: Map[Local, Set[CoreStmt]], rhs: Map[Local, Set[CoreStmt]]): Map[Local, Set[CoreStmt]] = {
      rhs.foldLeft(lhs) {
        case (m,(k,v)) => m + (k -> (m.getOrElse(k,Set.empty[CoreStmt]) ++ v))
      }
    }

    override val bottom: Map[Local, Set[CoreStmt]] = Map.empty
  }
  implicit val absTransfer : AbstractTransferable[Map[Local,Set[CoreStmt]]] = new AbstractTransferable[Map[Local, Set[CoreStmt]]] {

    override protected def transferInvoke(stmt: Invoke, input: Map[Local, Set[CoreStmt]]): Map[Local, Set[CoreStmt]] = {
      if(stmt.ret.nonEmpty) input + (stmt.ret.get -> Set(stmt))
      else input
    }

    override protected def transferIdentity(stmt: Identity, input: Map[Local, Set[CoreStmt]]): Map[Local, Set[CoreStmt]] = input + (stmt.lv -> Set(stmt))

    override protected def transferAssign(stmt: Assign, input: Map[Local, Set[CoreStmt]]): Map[Local, Set[CoreStmt]] = {
      if(stmt.lv.isInstanceOf[Local]) input + (stmt.lv.asInstanceOf[Local] -> Set(stmt))
      else input
    }
  }
}