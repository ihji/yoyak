package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.MemDom._

class MemDom[V](implicit val valueOps : LatticeOps[V]) {
  private var rawMap = Map.empty[AbsAddr,AbsValue[V]]
}

object MemDom {
  def empty[V](implicit valueOps : LatticeOps[V]) = new MemDom[V]
  def ops[V](implicit valueOps : LatticeOps[V]) = new LatticeOps[MemDom[V]] {
    override def <=(lhs: MemDom[V], rhs: MemDom[V]): Boolean = ???

    override def \/(lhs: MemDom[V], rhs: MemDom[V]): MemDom[V] = ???

    override val bottom: MemDom[V] = MemDom.empty[V]
  }

  case class AbsAddr(id: String)

  abstract class AbsValue[D]
  class AbsObject extends AbsValue
  case class AbsBox[D](data: D) extends AbsValue[D]
}