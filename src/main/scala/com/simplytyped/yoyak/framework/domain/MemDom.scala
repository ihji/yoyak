package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.MemDom._
import com.simplytyped.yoyak.il.CommonIL.ClassName
import com.simplytyped.yoyak.il.CommonIL.Value._

class MemDom[V](implicit val valueOps : LatticeOps[V]) {
  private var rawMap = MapDom.empty[AbsAddr,AbsValue[V]](AbsValue.ops[V])
  def update(kv: (Loc,AbsValue[V])) : MemDom[V] = {
    val (loc,v) = kv
    val newRawMap = loc match {
      case Local(id) => rawMap.update(AbsAddr(id)->v)
      case ArrayRef(base,index) => ???
      case InstanceFieldRef(base,field) =>
        val objRef = get(base)
        objRef match {
          case AbsRef(ids) =>
            ids.foldLeft(rawMap) {
              (m,i) =>
                val obj = rawMap.get(AbsAddr(i))
                val newObj = obj match {
                  case obj: AbsObject[V] =>
                    if(ids.size == 1) obj.updateField(field->v)
                    else obj.weakUpdateField(field->v)
                  case _ => obj // error case: should be reported
                }
                m.update(AbsAddr(i)->newObj)
            }
          case _ => rawMap // error case: should be reported
        }
      case StaticFieldRef(clazz,field) =>
        val staticAddr = getStaticAddr(clazz)
        val staticObj = rawMap.get(staticAddr)
        val newStaticObj = staticObj match {
          case obj: AbsObject[V] =>
            obj.weakUpdateField(field->v)
          case _ =>
            val obj = new AbsObject[V]
            obj.updateField(field->v)
        }
        rawMap.update(staticAddr->newStaticObj)
    }
    val newMemDom = new MemDom[V]
    newMemDom.rawMap = newRawMap
    newMemDom
  }
  def get(k: Loc) : AbsValue[V] =
    k match {
      case Local(id) => rawMap.get(AbsAddr(id))
      case ArrayRef(base,index) => ???
      case InstanceFieldRef(base,field) =>
        val objRef = get(base)
        objRef match {
          case AbsRef(ids) =>
            ids.foldLeft(AbsBottom: AbsValue[V]) {
              (v,id) =>
                val obj = rawMap.get(AbsAddr(id))
                val fieldValue = obj match {
                  case obj: AbsObject[V] => obj.getField(field)
                  case _ => AbsBottom
                }
                AbsValue.ops[V].\/(v,fieldValue)
            }
          case _ => AbsBottom
        }
      case StaticFieldRef(clazz,field) =>
        val staticAddr = getStaticAddr(clazz)
        val obj = rawMap.get(staticAddr)
        obj match {
          case obj: AbsObject[V] => obj.getField(field)
          case _ => AbsBottom
        }
    }
}

object MemDom {
  def empty[V](implicit valueOps : LatticeOps[V]) = new MemDom[V]

  def getStaticAddr(className: ClassName) : AbsAddr = AbsAddr(s"__static_obj_${ClassName.toString(className)}")

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
    def updateField(kv: (String,AbsValue[D])) = {
      val newFieldMap = rawFieldMap.update(kv)
      val newObject = new AbsObject[D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }
    def weakUpdateField(kv: (String,AbsValue[D])) = {
      val newFieldMap = rawFieldMap.weakUpdate(kv)
      val newObject = new AbsObject[D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }
    def getField(k: String) : AbsValue[D] = rawFieldMap.get(k)
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