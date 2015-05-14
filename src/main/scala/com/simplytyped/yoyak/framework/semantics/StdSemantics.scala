package com.simplytyped.yoyak.framework.semantics

import com.simplytyped.yoyak.framework.domain.{Galois, ArithmeticOps}
import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain.mem.MemDomLike
import com.simplytyped.yoyak.framework.domain.mem.MemElems.{AbsRef, AbsTop, AbsArith, AbsValue}
import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Value

trait StdSemantics[A<:Galois,D,Mem<:MemDomLike[A,D,Mem]] extends AbstractTransferable[GaloisIdentity[Mem]] {
  val arithOps : ArithmeticOps[A]

  override protected def transferIdentity(stmt: Identity, input: Mem) : Mem = {
    val (rv,output) = eval(stmt.rv,input)
    output.update(stmt.lv,rv)
  }
  override protected def transferAssign(stmt: Assign, input: Mem) : Mem = {
    val (rv,output) = eval(stmt.rv,input)
    output.update(stmt.lv,rv)
  }
  override protected def transferInvoke(stmt: Invoke, input: Mem) : Mem = input
  override protected def transferIf(stmt: If, input: Mem) : Mem = input
  override protected def transferAssume(stmt: Assume, input: Mem) : Mem = input
  override protected def transferReturn(stmt: Return, input: Mem) : Mem = input
  override protected def transferNop(stmt: Nop, input: Mem) : Mem = input
  override protected def transferGoto(stmt: Goto, input: Mem) : Mem = input
  override protected def transferEnterMonitor(stmt: EnterMonitor, input: Mem) : Mem = input
  override protected def transferExitMonitor(stmt: ExitMonitor, input: Mem) : Mem = input
  override protected def transferThrow(stmt: Throw, input: Mem) : Mem = input

  def eval(v: Value.t, input: Mem) : (AbsValue[A,D],Mem) = {
    v match {
      case x : Value.Constant => evalConstant(x,input)
      case x : Value.Loc => evalLoc(x,input)
      case x : Value.BinExp => evalBinExp(x,input)
      case Value.This => (AbsRef(Set("$this")),input)
      case Value.CaughtExceptionRef => (AbsRef(Set("$caughtex")),input)
      case Value.CastExp(v, ofTy) => evalLoc(v,input)
      case Value.InstanceOfExp(v, ofTy) => (AbsTop,input)
      case Value.LengthExp(v) => (AbsTop,input)
      case Value.NewExp(ofTy) => input.alloc
      case Value.NewArrayExp(ofTy, size) => input.alloc
    }
  }
  protected def evalConstant(v: Value.Constant, input: Mem) : (AbsValue[A,D],Mem) = (AbsArith[A](arithOps.lift(v)),input)

  protected def evalLoc(l: Value.Loc, input: Mem) : (AbsValue[A,D],Mem) = (input.get(l),input)

  protected def evalBinExp(binExp: Value.BinExp, input: Mem) : (AbsValue[A,D],Mem) = {
    binExp match {
      case e : Value.CompBinExp => evalCompBinExp(e,input)
      case e : Value.CondBinExp => evalCondBinExp(e,input)
    }
  }
  protected def evalCompBinExp(compExp: Value.CompBinExp, input: Mem) : (AbsValue[A,D],Mem) = {
    val (lv,output1) = eval(compExp.lv,input)
    val (rv,output2) = eval(compExp.rv,output1)
    val result = (lv,rv) match {
      case (AbsArith(d1),AbsArith(d2)) =>
        import Value.BinOp
        compExp.op match {
          case BinOp.+ => AbsArith(arithOps.+(d1,d2))
          case BinOp.- => AbsArith(arithOps.-(d1,d2))
          case BinOp.* => AbsArith(arithOps.*(d1,d2))
          case BinOp./ => AbsArith(arithOps./(d1,d2))
          case BinOp.&& => AbsTop
          case BinOp.|| => AbsTop
          case BinOp.& => AbsTop
          case BinOp.| => AbsTop
          case BinOp.>> => AbsTop
          case BinOp.<< => AbsTop
          case BinOp.>>> => AbsTop
          case BinOp.% => AbsTop
          case BinOp.^ => AbsTop
          case BinOp.cmp => AbsTop
          case BinOp.cmpl => AbsTop
          case BinOp.cmpg => AbsTop
        }
      case _ => AbsTop
    }
    (result,output2)
  }
  protected def evalCondBinExp(condExp: Value.CondBinExp, input: Mem) : (AbsValue[A,D],Mem) = {
    (AbsTop,input)
  }
}
