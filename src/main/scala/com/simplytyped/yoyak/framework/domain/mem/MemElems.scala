package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain._

object MemElems {
  case class AbsAddr(id: String) {
    def toAbsRef : AbsRef = AbsRef(Set(id))
  }

  abstract class AbsValue[+A,+D]

  case class AbsRef(id: Set[String]) extends AbsValue
  case class AbsArith[A<:Galois](data: A#Abst) extends AbsValue[A,Nothing]
  case class AbsBox[D<:Galois](data: D#Abst) extends AbsValue[Nothing,D]
  case object AbsBottom extends AbsValue
  case object AbsTop extends AbsValue

  object AbsValue {
    def ops[A <: Galois : ArithmeticOps, D <: Galois : LatticeWithTopOps] = new LatticeOps[GaloisIdentity[AbsValue[A,D]]] {
      val boxOps   = implicitly[LatticeWithTopOps[D]]
      val arithOps = implicitly[ArithmeticOps[A]]
      override def <=(lhs: AbsValue[A,D], rhs: AbsValue[A,D]): Option[Boolean] = {
        (lhs,rhs) match {
          case (AbsBottom,_) => Some(true)
          case (_,AbsBottom) => Some(false)
          case (_,AbsTop) => Some(true)
          case (AbsTop,_) => Some(false)
          case (AbsRef(id1),AbsRef(id2)) =>
            if(id1 subsetOf id2) Some(true)
            else if(id2 subsetOf id1) Some(false)
            else None
          case (AbsBox(data1),AbsBox(data2)) => boxOps.<=(data1,data2)
          case (AbsArith(data1),AbsArith(data2)) => arithOps.<=(data1,data2)
          case (_,_) => None
        }
      }

      override def \/(lhs: AbsValue[A,D], rhs: AbsValue[A,D]): AbsValue[A,D] = {
        (lhs,rhs) match {
          case (AbsBottom,x) => x
          case (x,AbsBottom) => x
          case (AbsTop,_) => AbsTop
          case (_,AbsTop) => AbsTop
          case (AbsRef(id1),AbsRef(id2)) => AbsRef(id1++id2)
          case (AbsBox(data1),AbsBox(data2)) =>
            val joined = boxOps.\/(data1,data2)
            if(boxOps.isTop(joined)) AbsTop
            else AbsBox(joined)
          case (AbsArith(data1),AbsArith(data2)) =>
            val joined = arithOps.\/(data1,data2)
            if(arithOps.isTop(joined)) AbsTop
            else AbsArith(joined)
          case (_,_) => AbsTop
        }
      }

      override val bottom: AbsValue[A,D] = AbsBottom
    }
  }
}
