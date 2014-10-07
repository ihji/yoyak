package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.{ArithmeticOps, LatticeOps, MapDom, ParOrdOps}
import com.simplytyped.yoyak.framework.domain.mem.MemDom._
import com.simplytyped.yoyak.framework.domain.mem.MemElems._
import com.simplytyped.yoyak.il.CommonIL.ClassName
import com.simplytyped.yoyak.il.CommonIL.Value._

class MemDom[A : ArithmeticOps, D : ParOrdOps] extends StdObjectModel[A,D,MemDom[A,D]] {

  val arithOps: ArithmeticOps[A] = implicitly[ArithmeticOps[A]]
  val parOrOps: ParOrdOps[D]     = implicitly[ParOrdOps[D]]

  override protected def builder(rawMap: MapDom[AbsAddr, AbsValue[A, D]]): MemDom[A, D] = {
    val newMemDom = new MemDom[A,D]
    newMemDom.rawMap = rawMap
    newMemDom
  }
}

object MemDom {
  def empty[A : ArithmeticOps, D : ParOrdOps] = new MemDom[A,D]

  def ops[A : ArithmeticOps, D : ParOrdOps] = new LatticeOps[MemDom[A,D]] {
    implicit val absValueOps = AbsValue.ops[A,D]
    override def <=(lhs: MemDom[A,D], rhs: MemDom[A,D]): Option[Boolean] = {
      MapDom.ops[AbsAddr,AbsValue[A,D]].<=(lhs.rawMap,rhs.rawMap)
    }

    override def \/(lhs: MemDom[A,D], rhs: MemDom[A,D]): MemDom[A,D] = {
      val newRawMap = MapDom.ops[AbsAddr,AbsValue[A,D]].\/(lhs.rawMap,rhs.rawMap)
      val newMemDom = new MemDom[A,D]
      newMemDom.rawMap = newRawMap
      newMemDom
    }

    override val bottom: MemDom[A,D] = MemDom.empty[A,D]
  }
}