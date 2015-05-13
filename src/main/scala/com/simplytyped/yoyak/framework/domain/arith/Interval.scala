package com.simplytyped.yoyak.framework.domain.arith

import com.simplytyped.yoyak.framework.domain.arith.Interval._

sealed abstract class Interval
case class Interv private (lb: IntWithInfMinus, ub: IntWithInf) extends Interval {
  assert(
      ub == IInf || lb == IInfMinus ||
      (lb.isInstanceOf[IInt] && ub.isInstanceOf[IInt] && lb.asInstanceOf[IInt].v <= ub.asInstanceOf[IInt].v)
  )
}
object Interv {
  def in(lb: IntWithInfMinus, ub: IntWithInf) : Interval = {
    if(lb == IInfMinus && ub == IInf) IntervTop
    else Interv(lb,ub)
  }
  def of(v: Int) : Interval = { Interv(IInt(v),IInt(v)) }
}
case object IntervBottom extends Interval
case object IntervTop extends Interval

object Interval {
  abstract class IntWithInfs extends Ordered[IntWithInfs] {
    override def compare(that: IntWithInfs): Int = {
      (this,that) match {
        case (IInf,IInf) => 0
        case (IInf,_) => 1
        case (IInfMinus,IInfMinus) => 0
        case (IInfMinus,_) => -1
        case (IInt(_),IInf) => -1
        case (IInt(_),IInfMinus) => 1
        case (IInt(v1),IInt(v2)) => v1 - v2
      }
    }
  }
  sealed trait IntWithInf extends IntWithInfs
  sealed trait IntWithInfMinus extends IntWithInfs

  case class IInt(v: Int) extends IntWithInf with IntWithInfMinus
  case object IInf extends IntWithInf
  case object IInfMinus extends IntWithInfMinus
}