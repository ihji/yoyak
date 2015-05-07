package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain.{Galois, LatticeWithTopOps, ArithmeticOps, MapDom}
import com.simplytyped.yoyak.framework.domain.mem.MemElems._
import com.simplytyped.yoyak.il.CommonIL.ClassName
import com.simplytyped.yoyak.il.CommonIL.Value._
import StdObjectModel._

trait StdObjectModel[A<:Galois,D<:Galois,This<:StdObjectModel[A,D,This]] extends MemDomLike[A,D,This] with ArrayJoinModel[A,D,This] {
  implicit val arithOps : ArithmeticOps[A]
  implicit val boxedOps : LatticeWithTopOps[D]

  protected[mem] var rawMap = MapDom.empty[AbsAddr,GaloisIdentity[AbsValue[A,D]]](AbsValue.ops[A,D])

  def alloc(loc: Loc) : This = {
    val newObj = new AbsObject[A,D]
    val newAddr = getNewAddr()
    val newRawMap = updateRawMap(rawMap,loc->newAddr.toAbsRef).update(newAddr->newObj)
    builder(newRawMap)
  }
  def remove(loc: Local) : This = {
    val newRawMap = rawMap.remove(AbsAddr(loc.id))
    builder(newRawMap)
  }
  private def updateRawMap(map: MapDom[AbsAddr,GaloisIdentity[AbsValue[A,D]]], kv: (Loc,AbsValue[A,D])) : MapDom[AbsAddr,GaloisIdentity[AbsValue[A,D]]] = {
    val (loc,v) = kv
    loc match {
      case Local(id) => map.update(AbsAddr(id)->v)
      case loc : ArrayRef => updateArray(loc,v).rawMap
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
      case Param(i) =>
        val addr = getParamAddr(i)
        map.update(addr->v)
    }
  }
  def update(kv: (Loc,AbsValue[A,D])) : This = {
    val newRawMap = updateRawMap(rawMap,kv)
    builder(newRawMap)
  }
  def get(k: Loc) : AbsValue[A,D] =
    k match {
      case Local(id) => rawMap.get(AbsAddr(id))
      case k : ArrayRef => getArray(k)
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
      case Param(i) =>
        val addr = getParamAddr(i)
        rawMap.get(addr)
    }
  def isStaticAddr(addr: AbsAddr) : Boolean = addr.id.startsWith(StdObjectModel.staticPrefix)
  def isDynamicAddr(addr: AbsAddr) : Boolean = addr.id.startsWith(StdObjectModel.dynamicPrefix)

  protected def builder(rawMap: MapDom[AbsAddr,GaloisIdentity[AbsValue[A,D]]]) : This
}

object StdObjectModel {
  val staticPrefix = "__static_obj_"
  val dynamicPrefix = "__dynamic_obj_"

  def getStaticAddr(className: ClassName) : AbsAddr = AbsAddr(s"$staticPrefix${ClassName.toString(className)}")

  var addrIdx = 0
  def getNewAddr() : AbsAddr = {addrIdx += 1; AbsAddr(s"$dynamicPrefix$addrIdx")}
  def getParamAddr(i: Int) : AbsAddr = AbsAddr(s"${dynamicPrefix}p$i")
}