package com.simplytyped.yoyak.framework.domain.arith

import com.simplytyped.yoyak.framework.domain.ArithmeticOps
import com.simplytyped.yoyak.framework.domain.arith.Interval._
import com.simplytyped.yoyak.il.CommonIL.Value.Constant

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
}
case object IntervBottom extends Interval
case object IntervTop extends Interval

object Interval {
  sealed trait IntWithInf
  sealed trait IntWithInfMinus

  case class IInt(v: Int) extends IntWithInf with IntWithInfMinus
  case object IInf extends IntWithInf
  case object IInfMinus extends IntWithInfMinus

  implicit val arithOps : ArithmeticOps[Interval] = new ArithmeticOps[Interval] {
    override def +(lhs: Interval, rhs: Interval): Interval = {
      (lhs,rhs) match {
        case (IntervBottom,_) => IntervBottom
        case (_,IntervBottom) => IntervBottom
        case (IntervTop,_) => IntervTop
        case (_,IntervTop) => IntervTop
        case (Interv(lb1,ub1),Interv(lb2,ub2)) =>
          val newlb = (lb1,lb2) match { /* lb1 + lb2 */
            case (IInfMinus,_) => IInfMinus
            case (_,IInfMinus) => IInfMinus
            case (IInt(v1),IInt(v2)) => IInt(v1+v2)
          }
          val newub = (ub1,ub2) match { /* ub1 + ub2 */
            case (IInf,_) => IInf
            case (_,IInf) => IInf
            case (IInt(v1),IInt(v2)) => IInt(v1+v2)
          }
          Interv.in(newlb,newub)
      }
    }

    override def /(lhs: Interval, rhs: Interval): Interval = ???

    override def lift(const: Constant): Interval = ???

    override def -(lhs: Interval, rhs: Interval): Interval = ???

    override def *(lhs: Interval, rhs: Interval): Interval = ???

    override def unlift[T: Numeric](abs: Interval): Option[Set[T]] = ???

    override def isTop(v: Interval): Boolean = ???

    override def bottom: Interval = ???

    override def \/(lhs: Interval, rhs: Interval): Interval = ???

    override def <=(lhs: Interval, rhs: Interval): Option[Boolean] = ???
  }
}