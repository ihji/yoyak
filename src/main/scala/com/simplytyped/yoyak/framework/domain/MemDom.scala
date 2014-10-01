package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.MemDom._

class MemDom[V](implicit val valueOps : LatticeOps[V]) {
  private var rawMap = MapDom.empty[AbsAddr,AbsValue[V]](AbsValue.ops[V])
  def update(kv: (AbsAddr,AbsValue[V])) : MemDom[V] = {
    val newMemDom = new MemDom[V]
    newMemDom.rawMap = rawMap.update(kv)
    newMemDom
  }
  def weakUpdate(kv: (AbsAddr,AbsValue[V])) : MemDom[V] = {
    val newMemDom = new MemDom[V]
    newMemDom.rawMap = rawMap.weakUpdate(kv)
    newMemDom
  }
}

object MemDom {
  def empty[V](implicit valueOps : LatticeOps[V]) = new MemDom[V]

  def ops[V](implicit valueOps : LatticeOps[V]) = new LatticeOps[MemDom[V]] {
    implicit val absValueOps = AbsValue.ops[V]
    override def <=(lhs: MemDom[V], rhs: MemDom[V]): Boolean = {
      MapDom.ops[AbsAddr,AbsValue[V]].<=(lhs.rawMap,rhs.rawMap)
    }

    override def \/(lhs: MemDom[V], rhs: MemDom[V]): MemDom[V] = {
      val newRawMap = MapDom.ops[AbsAddr,AbsValue[V]].\/(lhs.rawMap,rhs.rawMap)
      val newMemDom = new MemDom[V]
      newMemDom.rawMap = newRawMap
      newMemDom
    }

    override val bottom: MemDom[V] = MemDom.empty[V]
  }

  case class AbsAddr(id: String)

  abstract class AbsValue[+D]
  class AbsObject[D](implicit val valueOps : LatticeOps[D]) extends AbsValue {
    implicit val absValueOps = AbsValue.ops[D]
    private var rawFieldMap = MapDom.empty[String,AbsValue[D]]
    def \/(that: AbsObject[D]) : AbsObject[D] = {
      val newFieldMap = MapDom.ops[String,AbsValue[D]].\/(rawFieldMap,that.rawFieldMap)
      val newObject = new AbsObject[D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }
    def <=(that: AbsObject[D]) : Boolean = {
      MapDom.ops[String,AbsValue[D]].<=(rawFieldMap,that.rawFieldMap)
    }
  }
  case class AbsRef(id: Set[String]) extends AbsValue
  case class AbsBox[D](data: D) extends AbsValue[D]
  case object AbsBottom extends AbsValue
  case object AbsTop extends AbsValue

  object AbsValue {
    def ops[V](implicit valueOps : LatticeOps[V]) = new LatticeOps[AbsValue[V]] {
      override def <=(lhs: AbsValue[V], rhs: AbsValue[V]): Boolean = {
        (lhs,rhs) match {
          case (AbsBottom,_) => true
          case (_,AbsBottom) => false
          case (_,AbsTop) => true
          case (AbsTop,_) => false
          case (AbsRef(id1),AbsRef(id2)) => id1.subsetOf(id2)
          case (AbsBox(data1),AbsBox(data2)) => valueOps.<=(data1,data2)
          case (x,y) if x.isInstanceOf[AbsObject[V]] && y.isInstanceOf[AbsObject[V]] =>
            x.asInstanceOf[AbsObject[V]] <= y.asInstanceOf[AbsObject[V]]
          case (_,_) => false
        }
      }

      override def \/(lhs: AbsValue[V], rhs: AbsValue[V]): AbsValue[V] = {
        (lhs,rhs) match {
          case (AbsBottom,x) => x
          case (x,AbsBottom) => x
          case (AbsTop,_) => AbsTop
          case (_,AbsTop) => AbsTop
          case (AbsRef(id1),AbsRef(id2)) => AbsRef(id1++id2)
          case (AbsBox(data1),AbsBox(data2)) => AbsBox(valueOps.\/(data1,data2))
          case (x,y) if x.isInstanceOf[AbsObject[V]] && y.isInstanceOf[AbsObject[V]] =>
            x.asInstanceOf[AbsObject[V]] \/ y.asInstanceOf[AbsObject[V]]
          case (_,_) => AbsTop
        }
      }

      override val bottom: AbsValue[V] = AbsBottom
    }
  }
}