package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain._
import com.simplytyped.yoyak.framework.semantics.Widening
import com.simplytyped.yoyak.il.cfg.BasicBlock

object MemElems {
  case class AbsAddr(id: String) {
    def toAbsRef: AbsRef = AbsRef(Set(id))
  }

  abstract class AbsValue[+A, +D]

  case class AbsRef(id: Set[String])             extends AbsValue
  case class AbsArith[A <: Galois](data: A#Abst) extends AbsValue[A, Nothing]
  case class AbsBox[D <: Galois](data: D#Abst)   extends AbsValue[Nothing, D]
  case object AbsBottom                          extends AbsValue
  case object AbsTop                             extends AbsValue

  object AbsValue {
    def widening[A <: Galois: Widening, D <: Galois: Widening] =
      new Widening[GaloisIdentity[AbsValue[A, D]]] {
        val wideningA = implicitly[Widening[A]]
        val wideningD = implicitly[Widening[D]]
        override def <>(
            x: AbsValue[A, D],
            y: AbsValue[A, D]
        ): AbsValue[A, D] = {
          (x, y) match {
            case (AbsArith(a1), AbsArith(a2)) => AbsArith(wideningA.<>(a1, a2))
            case (AbsBox(d1), AbsBox(d2))     => AbsBox(wideningD.<>(d1, d2))
            case (_, _)                       => y
          }
        }
      }
    def ops[A <: Galois: ArithmeticOps, D <: Galois: LatticeWithTopOps] =
      new LatticeOps[GaloisIdentity[AbsValue[A, D]]] {
        val boxOps   = implicitly[LatticeWithTopOps[D]]
        val arithOps = implicitly[ArithmeticOps[A]]
        override def partialCompare(
            lhs: AbsValue[A, D],
            rhs: AbsValue[A, D]
        ): Double = {
          (lhs, rhs) match {
            case (AbsBottom, AbsBottom) => 0.0
            case (AbsBottom, _)         => -1.0
            case (_, AbsBottom)         => 1.0
            case (AbsTop, AbsTop)       => 0.0
            case (_, AbsTop)            => -1.0
            case (AbsTop, _)            => 1.0
            case (AbsRef(id1), AbsRef(id2)) =>
              if (id1 == id2) 0.0
              else if (id1 subsetOf id2) -1.0
              else if (id2 subsetOf id1) 1.0
              else Double.NaN
            case (AbsBox(data1), AbsBox(data2)) =>
              boxOps.partialCompare(data1, data2)
            case (AbsArith(data1), AbsArith(data2)) =>
              arithOps.partialCompare(data1, data2)
            case (_, _) => Double.NaN
          }
        }

        override def \/(
            lhs: AbsValue[A, D],
            rhs: AbsValue[A, D]
        ): AbsValue[A, D] = {
          (lhs, rhs) match {
            case (AbsBottom, x)             => x
            case (x, AbsBottom)             => x
            case (AbsTop, _)                => AbsTop
            case (_, AbsTop)                => AbsTop
            case (AbsRef(id1), AbsRef(id2)) => AbsRef(id1 ++ id2)
            case (AbsBox(data1), AbsBox(data2)) =>
              val joined = boxOps.\/(data1, data2)
              if (boxOps.isTop(joined)) AbsTop
              else AbsBox(joined)
            case (AbsArith(data1), AbsArith(data2)) =>
              val joined = arithOps.\/(data1, data2)
              if (arithOps.isTop(joined)) AbsTop
              else AbsArith(joined)
            case (_, _) => AbsTop
          }
        }

        override val bottom: AbsValue[A, D] = AbsBottom
      }
  }
}
