package com.simplytyped.yoyak.analysis

import com.simplytyped.yoyak.analysis.StringAnalysis.SetInt
import com.simplytyped.yoyak.android.AndroidAPIs
import com.simplytyped.yoyak.framework.ForwardAnalysis.FlowSensitiveForwardAnalysis
import com.simplytyped.yoyak.framework.domain.Galois.{SetAbstraction, GaloisIdentity}
import com.simplytyped.yoyak.framework.domain.mem.MemElems.{AbsTop, AbsBox}
import com.simplytyped.yoyak.framework.domain._
import com.simplytyped.yoyak.framework.domain.mem.MemDom
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable.Context
import com.simplytyped.yoyak.framework.semantics.StdSemantics
import com.simplytyped.yoyak.il.CommonIL.Statement.{Stmt, Invoke}
import com.simplytyped.yoyak.il.CommonIL.Value
import com.simplytyped.yoyak.il.CommonIL.Value.{Loc, StringConstant, Constant}
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

class StringAnalysis(cfg: CFG) {
  def run() : List[String] = {
    import StringAnalysis.{absTransfer,memDomOps}
    val analysis = new FlowSensitiveForwardAnalysis[GaloisIdentity[MemDom[SetInt,SetAbstraction[String]]]](cfg)
    val output = analysis.compute
    check(output)
  }
  def findHost(stmt: Stmt, mem: MemDom[SetInt,SetAbstraction[String]]) : List[String] = {
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
  def check(output: MapDom[BasicBlock,GaloisIdentity[MemDom[SetInt,SetAbstraction[String]]]]) : List[String] = {
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

  implicit val absTransfer : StdSemantics[SetInt,SetAbstraction[String],MemDom[SetInt,SetAbstraction[String]]] = new StdSemantics[SetInt,SetAbstraction[String],MemDom[SetInt,SetAbstraction[String]]] {
    val arithOps = StringAnalysis.arithOps
    override protected def evalConstant(v: Value.Constant, input: MemDom[SetInt,SetAbstraction[String]])(implicit context: Context) = {
      v match {
        case StringConstant(s) => (AbsBox[SetAbstraction[String]](Set(s)),input)
        case _ => (AbsTop,input)
      }
    }
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

    override def partialCompare(lhs: Set[Int], rhs: Set[Int]): Double =
      if(lhs == rhs) 0.0
      else if(lhs subsetOf rhs) -1.0
      else if(rhs subsetOf lhs) 1.0
      else Double.NaN
  }
  implicit val memDomOps : LatticeOps[GaloisIdentity[MemDom[SetInt,SetAbstraction[String]]]] = MemDom.ops[SetInt,SetAbstraction[String]]
}