package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain._
import com.simplytyped.yoyak.framework.domain.mem.MemElems._
import com.simplytyped.yoyak.framework.semantics.Widening

class MemDom[A, D](implicit
    val arithOps: ArithmeticOps[A],
    val boxedOps: LatticeWithTopOps[D]
) extends StdObjectModel[A, D, MemDom[A, D]] {

  override protected def builder(
      rawMap: MapDom[AbsAddr, AbsValue[A, D]]
  ): MemDom[A, D] = {
    val newMemDom = new MemDom[A, D]
    newMemDom.rawMap = rawMap
    newMemDom
  }
}

object MemDom {
  def empty[A: ArithmeticOps, D: LatticeWithTopOps] =
    new MemDom[A, D]

  def widening[
      A: Widening: ArithmeticOps,
      D: Widening: LatticeWithTopOps
  ] =
    new Widening[MemDom[A, D]] {
      implicit val wideningAbsValue: Widening[AbsValue[A, D]] =
        StdObjectModel.wideningWithObject[A, D]
      implicit val absValueOps: LatticeOps[AbsValue[A, D]] =
        StdObjectModel.absValueOpsWithObject[A, D]

      override def <>(x: MemDom[A, D], y: MemDom[A, D]): MemDom[A, D] = {
        val newRawMap = MapDom
          .widening[AbsAddr, AbsValue[A, D]]
          .<>(x.rawMap, y.rawMap)
        val newMemDom = new MemDom[A, D]
        newMemDom.rawMap = newRawMap
        newMemDom
      }
    }

  def ops[A: ArithmeticOps, D: LatticeWithTopOps] =
    new LatticeOps[MemDom[A, D]] {
      implicit val absValueOps: LatticeOps[AbsValue[A, D]] =
        StdObjectModel.absValueOpsWithObject[A, D]

      override def partialCompare(
          lhs: MemDom[A, D],
          rhs: MemDom[A, D]
      ): Double = {
        MapDom
          .ops[AbsAddr, AbsValue[A, D]]
          .partialCompare(lhs.rawMap, rhs.rawMap)
      }

      override def \/(lhs: MemDom[A, D], rhs: MemDom[A, D]): MemDom[A, D] = {
        val newRawMap = MapDom
          .ops[AbsAddr, AbsValue[A, D]]
          .\/(lhs.rawMap, rhs.rawMap)
        val newMemDom = new MemDom[A, D]
        newMemDom.rawMap = newRawMap
        newMemDom
      }

      override val bottom: MemDom[A, D] = MemDom.empty[A, D]
    }
}
