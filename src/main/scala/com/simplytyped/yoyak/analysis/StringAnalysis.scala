package com.simplytyped.yoyak.analysis

import com.simplytyped.yoyak.analysis.StringAnalysis.{SetString, SetInt}
import com.simplytyped.yoyak.android.AndroidAPIs
import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain.mem.MemElems.AbsBox
import com.simplytyped.yoyak.framework.domain._
import com.simplytyped.yoyak.framework.domain.mem.MemDom
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.CommonIL.Statement.{Stmt, Invoke, Assign}
import com.simplytyped.yoyak.il.CommonIL.Value.{Loc, StringConstant, Constant}
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

class StringAnalysis(cfg: CFG) {
  def run() : List[String] = {
    import StringAnalysis.{absTransfer,memDomOps}
    val analysis = new FlowSensitiveForwardAnalysis[GaloisIdentity[MemDom[SetInt,SetString]]](cfg)
    val output = analysis.compute
    check(output)
  }
  def findHost(stmt: Stmt, mem: MemDom[SetInt,SetString]) : List[String] = {
    stmt match {
      case Invoke(_,callee) if AndroidAPIs.internet(callee.callee) => callee.args.foldLeft(List.empty[String]) {
        case (list,v) =>
          v match {
            case l : Loc =>
              mem.get(l) match {
                case AbsBox(strs) => list ++ strs
                case _ => list
              }
            case _ => list
          }
      }
      case _ => List.empty[String]
    }
  }
  def check(output: MapDom[BasicBlock,GaloisIdentity[MemDom[SetInt,SetString]]]) : List[String] = {
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
  class SetInt extends Galois {
    type Conc = Int
    type Abst = Set[Int]
  }
  class SetString extends Galois {
    type Conc = String
    type Abst = Set[String]
  }

  implicit val absTransfer : AbstractTransferable[GaloisIdentity[MemDom[SetInt,SetString]]] = new AbstractTransferable[GaloisIdentity[MemDom[SetInt,SetString]]] {
    override protected def transferAssign(stmt: Assign, input: MemDom[SetInt, SetString]): MemDom[SetInt, SetString] = {
      stmt.rv match {
        case StringConstant(s) => input.update(stmt.lv->AbsBox[SetString](Set(s)))
        case _ => input
      }
    }
  }
  implicit val boxedOps : LatticeWithTopOps[SetString] = new LatticeWithTopOps[SetString] {
    override def isTop(v: Set[String]): Boolean = false

    override def bottom: Set[String] = Set.empty[String]

    override def \/(lhs: Set[String], rhs: Set[String]): Set[String] = lhs ++ rhs

    override def <=(lhs: Set[String], rhs: Set[String]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None
  }
  implicit val arithOps : ArithmeticOps[SetInt] = new ArithmeticOps[SetInt] {
    override def +(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def /(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def -(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def *(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def lift(const: Constant): Set[Int] = ???

    override def unlift(abs: Set[Int]): Option[Set[Int]] = ???

    override def isTop(v: Set[Int]): Boolean = false

    override def bottom: Set[Int] = Set.empty[Int]

    override def \/(lhs: Set[Int], rhs: Set[Int]): Set[Int] = lhs ++ rhs

    override def <=(lhs: Set[Int], rhs: Set[Int]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None
  }
  implicit val memDomOps : LatticeOps[GaloisIdentity[MemDom[SetInt,SetString]]] = MemDom.ops[SetInt,SetString]
}