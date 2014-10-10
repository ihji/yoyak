package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.{LatticeWithTopOps, LatticeOps, MapDom, ArithmeticOps}

object MemElems {
  case class AbsAddr(id: String) {
    def toAbsRef : AbsRef = AbsRef(Set(id))
  }

  abstract class AbsValue[+A,+D]
  class AbsObject[A : ArithmeticOps, D : LatticeWithTopOps] extends AbsValue {
    implicit val absValueOps = AbsValue.ops[A,D]
    private var rawFieldMap = MapDom.empty[String,AbsValue[A,D]]
    def \/(that: AbsObject[A,D]) : AbsObject[A,D] = {
      val newFieldMap = MapDom.ops[String,AbsValue[A,D]].\/(rawFieldMap,that.rawFieldMap)
      val newObject = new AbsObject[A,D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }
    def <=(that: AbsObject[A,D]) : Option[Boolean] = {
      MapDom.ops[String,AbsValue[A,D]].<=(rawFieldMap,that.rawFieldMap)
    }
    def updateField(kv: (String,AbsValue[A,D])) = {
      val newFieldMap = rawFieldMap.update(kv)
      val newObject = new AbsObject[A,D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }
    def weakUpdateField(kv: (String,AbsValue[A,D])) = {
      val newFieldMap = rawFieldMap.weakUpdate(kv)
      val newObject = new AbsObject[A,D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }
    def getField(k: String) : AbsValue[A,D] = rawFieldMap.get(k)
  }
  case class AbsRef(id: Set[String]) extends AbsValue
  case class AbsArith[A](data: A) extends AbsValue[A,Nothing]
  case class AbsBox[D](data: D) extends AbsValue[Nothing,D]
  case object AbsBottom extends AbsValue
  case object AbsTop extends AbsValue

  object AbsValue {
    def ops[A : ArithmeticOps, D : LatticeWithTopOps] = new LatticeOps[AbsValue[A,D]] {
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
          case (x,y) if x.isInstanceOf[AbsObject[A,D]] && y.isInstanceOf[AbsObject[A,D]] =>
            x.asInstanceOf[AbsObject[A,D]] <= y.asInstanceOf[AbsObject[A,D]]
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
          case (x,y) if x.isInstanceOf[AbsObject[A,D]] && y.isInstanceOf[AbsObject[A,D]] =>
            x.asInstanceOf[AbsObject[A,D]] \/ y.asInstanceOf[AbsObject[A,D]]
          case (_,_) => AbsTop
        }
      }

      override val bottom: AbsValue[A,D] = AbsBottom
    }
  }
}
