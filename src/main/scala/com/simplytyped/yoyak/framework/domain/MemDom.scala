package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.MemDom._
import com.simplytyped.yoyak.il.CommonIL.ClassName
import com.simplytyped.yoyak.il.CommonIL.Value._

class MemDom[A : ArithmeticOps, D : ParOrdOps] {
  private var rawMap = MapDom.empty[AbsAddr,AbsValue[A,D]](AbsValue.ops[A,D])
  def alloc(loc: Loc) : MemDom[A,D] = {
    val newObj = new AbsObject[A,D]
    val newAddr = getNewAddr()
    val newRawMap = updateRawMap(rawMap,loc->newAddr.toAbsRef).update(newAddr->newObj)
    builder(newRawMap)
  }
  private def updateRawMap(map: MapDom[AbsAddr,AbsValue[A,D]], kv: (Loc,AbsValue[A,D])) : MapDom[AbsAddr,AbsValue[A,D]] = {
    val (loc,v) = kv
    loc match {
      case Local(id) => map.update(AbsAddr(id)->v)
      case ArrayRef(base,index) => ???
      case InstanceFieldRef(base,field) =>
        val objRef = get(base)
        objRef match {
          case AbsRef(ids) =>
            ids.foldLeft(map) {
              (m,i) =>
                val obj = map.get(AbsAddr(i))
                val newObj = obj match {
                  case obj: AbsObject[A,D] =>
                    if(ids.size == 1) obj.updateField(field->v)
                    else obj.weakUpdateField(field->v)
                  case _ => obj // error case: should be reported
                }
                m.update(AbsAddr(i)->newObj)
            }
          case _ => map // error case: should be reported
        }
      case StaticFieldRef(clazz,field) =>
        val staticAddr = getStaticAddr(clazz)
        val staticObj = map.get(staticAddr)
        val newStaticObj = staticObj match {
          case obj: AbsObject[A,D] =>
            obj.weakUpdateField(field->v)
          case _ =>
            val obj = new AbsObject[A,D]
            obj.updateField(field->v)
        }
        map.update(staticAddr->newStaticObj)
    }
  }
  def update(kv: (Loc,AbsValue[A,D])) : MemDom[A,D] = {
    val newRawMap = updateRawMap(rawMap,kv)
    builder(newRawMap)
  }
  def get(k: Loc) : AbsValue[A,D] =
    k match {
      case Local(id) => rawMap.get(AbsAddr(id))
      case ArrayRef(base,index) => ???
      case InstanceFieldRef(base,field) =>
        val objRef = get(base)
        objRef match {
          case AbsRef(ids) =>
            ids.foldLeft(AbsBottom: AbsValue[A,D]) {
              (v,id) =>
                val obj = rawMap.get(AbsAddr(id))
                val fieldValue = obj match {
                  case obj: AbsObject[A,D] => obj.getField(field)
                  case _ => AbsBottom
                }
                AbsValue.ops[A,D].\/(v,fieldValue)
            }
          case _ => AbsBottom
        }
      case StaticFieldRef(clazz,field) =>
        val staticAddr = getStaticAddr(clazz)
        val obj = rawMap.get(staticAddr)
        obj match {
          case obj: AbsObject[A,D] => obj.getField(field)
          case _ => AbsBottom
        }
    }
  private def builder(rawMap: MapDom[AbsAddr,AbsValue[A,D]]) : MemDom[A,D] = {
    val newMemDom = new MemDom[A,D]
    newMemDom.rawMap = rawMap
    newMemDom
  }
}

object MemDom {
  def empty[A : ArithmeticOps, D : ParOrdOps] = new MemDom[A,D]

  def getStaticAddr(className: ClassName) : AbsAddr = AbsAddr(s"__static_obj_${ClassName.toString(className)}")

  var addrIdx = 0
  def getNewAddr() : AbsAddr = {addrIdx += 1; AbsAddr(s"__dynamic_obj_$addrIdx")}

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

  case class AbsAddr(id: String) {
    def toAbsRef : AbsRef = AbsRef(Set(id))
  }

  abstract class AbsValue[+A,+D]
  class AbsObject[A : ArithmeticOps, D : ParOrdOps] extends AbsValue {
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
    def ops[A : ArithmeticOps, D : ParOrdOps] = new LatticeOps[AbsValue[A,D]] {
      val boxOps = implicitly[ParOrdOps[D]]
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
            val order = boxOps.<=(data1,data2)
            if(order.nonEmpty) {
              if(order.get) AbsBox(data2) else AbsBox(data1)
            } else AbsTop
          case (AbsArith(data1),AbsArith(data2)) =>
            val order = arithOps.<=(data1,data2)
            if(order.nonEmpty) {
              if(order.get) AbsArith(data2) else AbsArith(data1)
            } else AbsTop
          case (x,y) if x.isInstanceOf[AbsObject[A,D]] && y.isInstanceOf[AbsObject[A,D]] =>
            x.asInstanceOf[AbsObject[A,D]] \/ y.asInstanceOf[AbsObject[A,D]]
          case (_,_) => AbsTop
        }
      }

      override val bottom: AbsValue[A,D] = AbsBottom
    }
  }
}