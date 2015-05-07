package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain._
import com.simplytyped.yoyak.framework.domain.mem.MemElems._

class MemDom[A <: Galois : ArithmeticOps, D <: Galois : LatticeWithTopOps] extends StdObjectModel[A,D,MemDom[A,D]] {

  lazy val arithOps: ArithmeticOps[A]     = implicitly[ArithmeticOps[A]]
  lazy val boxedOps: LatticeWithTopOps[D] = implicitly[LatticeWithTopOps[D]]

  override protected def builder(rawMap: MapDom[AbsAddr, GaloisIdentity[AbsValue[A, D]]]): MemDom[A, D] = {
    val newMemDom = new MemDom[A,D]
    newMemDom.rawMap = rawMap
    newMemDom
  }
}

object MemDom {
  def empty[A <: Galois : ArithmeticOps, D <: Galois : LatticeWithTopOps] = new MemDom[A,D]

  def ops[A <: Galois : ArithmeticOps, D <: Galois : LatticeWithTopOps] = new LatticeOps[GaloisIdentity[MemDom[A,D]]] {
    implicit val absValueOps = AbsValue.ops[A,D]
    override def <=(lhs: MemDom[A,D], rhs: MemDom[A,D]): Option[Boolean] = {
      MapDom.ops[AbsAddr,GaloisIdentity[AbsValue[A,D]]].<=(lhs.rawMap,rhs.rawMap)
    }

    override def \/(lhs: MemDom[A,D], rhs: MemDom[A,D]): MemDom[A,D] = {
      val newRawMap = MapDom.ops[AbsAddr,GaloisIdentity[AbsValue[A,D]]].\/(lhs.rawMap,rhs.rawMap)
      val newMemDom = new MemDom[A,D]
      newMemDom.rawMap = newRawMap
      newMemDom
    }

    override val bottom: MemDom[A,D] = MemDom.empty[A,D]
  }
}