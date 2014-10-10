package com.simplytyped.yoyak.analysis

import com.simplytyped.yoyak.android.AndroidAPIs
import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.mem.MemElems.AbsBox
import com.simplytyped.yoyak.framework.domain.{MapDom, LatticeOps, ArithmeticOps, LatticeWithTopOps}
import com.simplytyped.yoyak.framework.domain.mem.MemDom
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.CommonIL.Statement.{Stmt, Invoke, Assign}
import com.simplytyped.yoyak.il.CommonIL.Value.{Loc, StringConstant, Constant}
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

class StringAnalysis(cfg: CFG) {
  def run() : List[String] = {
    import StringAnalysis.{absTransfer,memDomOps}
    val analysis = new FlowSensitiveForwardAnalysis[MemDom[Set[Int],Set[String]]](cfg)
    val output = analysis.compute
    check(output)
  }
  def findHost(stmt: Stmt, mem: MemDom[Set[Int],Set[String]]) : List[String] = {
    stmt match {
      case Invoke(_,callee) if AndroidAPIs.internet(callee.callee) => callee.args.foldLeft(List.empty[String]) {
        case (list,v) =>
          v match {
            case l : Loc =>
              mem.get(l) match {
                case AbsBox(strs) => list ++ strs
              }
            case _ => list
          }
      }
      case _ => List.empty[String]
    }
  }
  def check(output: MapDom[BasicBlock,MemDom[Set[Int],Set[String]]]) : List[String] = {
    cfg.nodes.foldLeft(List.empty[String]) {
      (list,node) =>
        val prevs = cfg.getPrevs(node)
        val input = prevs.foldLeft(StringAnalysis.memDomOps.bottom){(m,p) => StringAnalysis.memDomOps.\/(m,output.get(p))}
        val hosts = node.data.getStmts.foldLeft(input,List.empty[String]) {
          case ((m,l),stmt) =>
            val hosts = findHost(stmt,m)
            val next = StringAnalysis.absTransfer.transfer(m,stmt)
            (next,l++hosts)
        }._2
        list++hosts
    }
  }
}

object StringAnalysis {
  implicit val absTransfer : AbstractTransferable[MemDom[Set[Int],Set[String]]] = new AbstractTransferable[MemDom[Set[Int],Set[String]]] {
    override protected def transferAssign(stmt: Assign, input: MemDom[Set[Int], Set[String]]): MemDom[Set[Int], Set[String]] = {
      stmt.rv match {
        case StringConstant(s) => input.update(stmt.lv->AbsBox(Set(s)))
        case _ => input
      }
    }
  }

  implicit val memDomOps : LatticeOps[MemDom[Set[Int],Set[String]]] = MemDom.ops[Set[Int],Set[String]]

  implicit val boxedOps = new LatticeWithTopOps[Set[String]] {
    override def isTop(v: Set[String]): Boolean = false

    override def bottom: Set[String] = Set.empty[String]

    override def \/(lhs: Set[String], rhs: Set[String]): Set[String] = lhs ++ rhs

    override def <=(lhs: Set[String], rhs: Set[String]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None
  }
  implicit val arithOps : ArithmeticOps[Set[Int]] = new ArithmeticOps[Set[Int]] {
    override def +(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def /(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def -(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def *(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def lift(const: Constant): Set[Int] = ???

    override def unlift[T: Numeric](abs: Set[Int]): Option[Set[T]] = ???

    override def isTop(v: Set[Int]): Boolean = false

    override def bottom: Set[Int] = Set.empty[Int]

    override def \/(lhs: Set[Int], rhs: Set[Int]): Set[Int] = lhs ++ rhs

    override def <=(lhs: Set[Int], rhs: Set[Int]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None
  }
}