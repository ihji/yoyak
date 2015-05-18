package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain._
import com.simplytyped.yoyak.framework.domain.mem.MemElems._
import com.simplytyped.yoyak.framework.semantics.Widening

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

  def widening[A<:Galois:Widening:ArithmeticOps,D<:Galois:Widening:LatticeWithTopOps] = new Widening[GaloisIdentity[MemDom[A,D]]] {
    implicit val wideningAbsValue = StdObjectModel.wideningWithObject[A,D]
    implicit val absValueOps = StdObjectModel.absValueOpsWithObject[A,D]

    override def <>(x: MemDom[A, D], y: MemDom[A, D]): MemDom[A, D] = {
      val newRawMap = MapDom.widening[AbsAddr,GaloisIdentity[AbsValue[A,D]]].<>(x.rawMap,y.rawMap)
      val newMemDom = new MemDom[A,D]
      newMemDom.rawMap = newRawMap
      newMemDom
    }
  }

  def ops[A <: Galois : ArithmeticOps, D <: Galois : LatticeWithTopOps] = new LatticeOps[GaloisIdentity[MemDom[A,D]]] {
    implicit val absValueOps = StdObjectModel.absValueOpsWithObject[A,D]

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