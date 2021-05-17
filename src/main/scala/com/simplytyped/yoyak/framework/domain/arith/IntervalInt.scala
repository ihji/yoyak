package com.simplytyped.yoyak.framework.domain.arith

import com.simplytyped.yoyak.framework.domain.arith.Interval._
import com.simplytyped.yoyak.framework.domain.{ArithmeticOps, Galois}
import com.simplytyped.yoyak.framework.semantics.Widening
import com.simplytyped.yoyak.il.CommonIL.Value.{IntegerConstant, Constant}

import scala.util.Sorting

class IntervalInt extends Galois {
  type Conc = Int
  type Abst = Interval
}

object IntervalInt {
  implicit val arithOps: ArithmeticOps[IntervalInt] =
    new ArithmeticOps[IntervalInt] {
      override def +(lhs: Interval, rhs: Interval): Interval = {
        (lhs, rhs) match {
          case (IntervBottom, _) => IntervBottom
          case (_, IntervBottom) => IntervBottom
          case (IntervTop, _)    => IntervTop
          case (_, IntervTop)    => IntervTop
          case (Interv(lb1, ub1), Interv(lb2, ub2)) =>
            val newlb = (lb1, lb2) match {
              /* lb1 + lb2 */
              case (IInfMinus, _)       => IInfMinus
              case (_, IInfMinus)       => IInfMinus
              case (IInt(v1), IInt(v2)) => IInt(v1 + v2)
            }
            val newub = (ub1, ub2) match {
              /* ub1 + ub2 */
              case (IInf, _)            => IInf
              case (_, IInf)            => IInf
              case (IInt(v1), IInt(v2)) => IInt(v1 + v2)
            }
            Interv.in(newlb, newub)
        }
      }

      override def /(lhs: Interval, rhs: Interval): Interval = {
        def divBound(a: IntWithInfs, b: IntWithInfs): Option[IntWithInfs] = {
          (a, b) match {
            case (IInf, IInf)      => Some(IInf)
            case (IInf, IInfMinus) => Some(IInfMinus)
            case (IInf, IInt(v)) =>
              if (v > 0) Some(IInf) else if (v == 0) None else Some(IInfMinus)
            case (IInfMinus, IInf)      => Some(IInfMinus)
            case (IInfMinus, IInfMinus) => Some(IInf)
            case (IInfMinus, IInt(v)) =>
              if (v > 0) Some(IInfMinus) else if (v == 0) None else Some(IInf)
            case (IInt(_), IInf)      => Some(IInt(0))
            case (IInt(_), IInfMinus) => Some(IInt(0))
            case (IInt(v1), IInt(v2)) =>
              if (v2 != 0) Some(IInt(v1 / v2)) else None
          }
        }
        (lhs, rhs) match {
          case (IntervBottom, _) => IntervBottom
          case (_, IntervBottom) => IntervBottom
          case (IntervTop, _)    => IntervTop
          case (_, IntervTop)    => IntervTop
          case (Interv(lb1, ub1), Interv(lb2, ub2)) =>
            val values = Array(
              divBound(lb1, lb2),
              divBound(lb1, ub2),
              divBound(ub1, lb2),
              divBound(ub1, ub2)
            ).flatten
            if (values.isEmpty) IntervBottom
            else {
              Sorting.quickSort(values)
              val newlb =
                values.head
                  .asInstanceOf[IntWithInfMinus] // this couldn't be IInf
              val newub =
                values.last
                  .asInstanceOf[IntWithInf] // this couldn't be IInfMinus
              Interv.in(newlb, newub)
            }
        }
      }

      override def lift(const: Constant): Interval = {
        const match {
          case IntegerConstant(v) => Interv.of(v)
          case _                  => IntervBottom // hmm...
        }
      }

      override def -(lhs: Interval, rhs: Interval): Interval = {
        (lhs, rhs) match {
          case (IntervBottom, _) => IntervBottom
          case (_, IntervBottom) => IntervBottom
          case (IntervTop, _)    => IntervTop
          case (_, IntervTop)    => IntervTop
          case (Interv(lb1, ub1), Interv(lb2, ub2)) =>
            val newlb = (lb1, ub2) match {
              /* lb1 - ub2 */
              case (IInfMinus, _)       => IInfMinus
              case (_, IInf)            => IInfMinus
              case (IInt(v1), IInt(v2)) => IInt(v1 - v2)
            }
            val newub = (ub1, lb2) match {
              /* ub1 - lb2 */
              case (IInf, _)            => IInf
              case (_, IInfMinus)       => IInf
              case (IInt(v1), IInt(v2)) => IInt(v1 - v2)
            }
            Interv.in(newlb, newub)
        }
      }

      override def *(lhs: Interval, rhs: Interval): Interval = {
        def multBound(a: IntWithInfs, b: IntWithInfs): IntWithInfs = {
          (a, b) match {
            case (IInf, IInf)      => IInf
            case (IInf, IInfMinus) => IInfMinus
            case (IInf, IInt(v)) =>
              if (v > 0) IInf else if (v == 0) IInt(0) else IInfMinus
            case (IInfMinus, IInf)      => multBound(b, a)
            case (IInfMinus, IInfMinus) => IInf
            case (IInfMinus, IInt(v)) =>
              if (v > 0) IInfMinus else if (v == 0) IInt(0) else IInf
            case (IInt(_), IInf)      => multBound(b, a)
            case (IInt(_), IInfMinus) => multBound(b, a)
            case (IInt(v1), IInt(v2)) => IInt(v1 * v2)
          }
        }
        (lhs, rhs) match {
          case (IntervBottom, _) => IntervBottom
          case (_, IntervBottom) => IntervBottom
          case (IntervTop, _)    => IntervTop
          case (_, IntervTop)    => IntervTop
          case (Interv(lb1, ub1), Interv(lb2, ub2)) =>
            val values = Array(
              multBound(lb1, lb2),
              multBound(lb1, ub2),
              multBound(ub1, lb2),
              multBound(ub1, ub2)
            )
            Sorting.quickSort(values)
            val newlb =
              values.head.asInstanceOf[IntWithInfMinus] // this couldn't be IInf
            val newub =
              values.last.asInstanceOf[IntWithInf] // this couldn't be IInfMinus
            Interv.in(newlb, newub)
        }
      }

      override def unlift(abs: Interval): Option[Set[Int]] = {
        abs match {
          case IntervBottom               => Some(Set.empty)
          case Interv(IInt(v1), IInt(v2)) => Some((v1 to v2).toSet)
          case _ =>
            None // impossible to concretize infinite sets. better ideas?
        }
      }

      override def isTop(v: Interval): Boolean = { v == IntervTop }

      override def bottom: Interval = IntervBottom

      override def \/(lhs: Interval, rhs: Interval): Interval =
        (lhs, rhs) match {
          case (IntervTop, _)    => IntervTop
          case (_, IntervTop)    => IntervTop
          case (IntervBottom, x) => x
          case (x, IntervBottom) => x
          case (Interv(lb1, ub1), Interv(lb2, ub2)) =>
            val newLb = (lb1, lb2) match {
              case (IInfMinus, _)       => IInfMinus
              case (_, IInfMinus)       => IInfMinus
              case (IInt(v1), IInt(v2)) => IInt(Math.min(v1, v2))
            }
            val newUb = (ub1, ub2) match {
              case (IInf, _)            => IInf
              case (_, IInf)            => IInf
              case (IInt(v1), IInt(v2)) => IInt(Math.max(v1, v2))
            }
            Interv.in(newLb, newUb)
        }

      override def partialCompare(lhs: Interval, rhs: Interval): Double =
        (lhs, rhs) match {
          case (IntervTop, IntervTop)       => 0.0
          case (_, IntervTop)               => -1.0
          case (IntervTop, _)               => 1.0
          case (IntervBottom, IntervBottom) => 0.0
          case (IntervBottom, _)            => -1.0
          case (_, IntervBottom)            => 1.0
          case (Interv(lb1, ub1), Interv(lb2, ub2)) =>
            val lbOrder = (lb1, lb2) match {
              case (IInfMinus, IInfMinus) => 0
              case (IInfMinus, _)         => -1
              case (_, IInfMinus)         => 1
              case (IInt(v1), IInt(v2))   => v1 compare v2
            }
            val ubOrder = (ub1, ub2) match {
              case (IInf, IInf)         => 0
              case (IInf, _)            => 1
              case (_, IInf)            => -1
              case (IInt(v1), IInt(v2)) => v1 compare v2
            }
            (lbOrder, ubOrder) match {
              case (x, y) if x == 0 && y == 0 => 0.0
              case (x, y) if x >= 0 && y <= 0 => -1.0
              case (x, y) if x <= 0 && y >= 0 => 1.0
              case _                          => Double.NaN
            }
        }
    }
  implicit val widening = new Widening[IntervalInt] {
    override def <>(x: Interval, y: Interval): Interval = {
      (x, y) match {
        case (IntervTop, _)    => IntervTop
        case (_, IntervTop)    => IntervTop
        case (IntervBottom, x) => x
        case (_, IntervBottom) => IntervBottom
        case (Interv(lb1, ub1), Interv(lb2, ub2)) =>
          val newLb = (lb1, lb2) match {
            case (IInt(v1), IInt(v2)) if v1 <= v2 => IInt(v1)
            case _                                => IInfMinus
          }
          val newUb = (ub1, ub2) match {
            case (IInt(v1), IInt(v2)) if v2 <= v1 => IInt(v1)
            case _                                => IInf
          }
          Interv.in(newLb, newUb)
      }
    }
  }
}
