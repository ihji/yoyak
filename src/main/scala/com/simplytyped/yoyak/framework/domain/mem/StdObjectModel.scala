package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.domain._
import com.simplytyped.yoyak.framework.domain.mem.MemElems._
import com.simplytyped.yoyak.framework.semantics.Widening
import com.simplytyped.yoyak.il.CommonIL.ClassName
import com.simplytyped.yoyak.il.CommonIL.Statement.Stmt
import com.simplytyped.yoyak.il.CommonIL.Value._
import StdObjectModel._

trait StdObjectModel[A <: Galois, D <: Galois, This <: StdObjectModel[
  A,
  D,
  This
]] extends MemDomLike[A, D, This]
    with ArrayJoinModel[A, D, This] {
  implicit val arithOps: ArithmeticOps[A]
  implicit val boxedOps: LatticeWithTopOps[D]

  implicit val absValueOps = absValueOpsWithObject[A, D]

  protected[mem] var rawMap =
    MapDom.empty[AbsAddr, GaloisIdentity[AbsValue[A, D]]]

  def alloc(from: Stmt): (AbsRef, This) = {
    val newObj    = new AbsObject[A, D]
    val newAddr   = getNewAddr(from)
    val newRawMap = rawMap.update(newAddr -> newObj)
    (newAddr.toAbsRef, builder(newRawMap))
  }
  def remove(loc: Local): This = {
    val newRawMap = rawMap.remove(AbsAddr(loc.id))
    builder(newRawMap)
  }
  private def updateRawMap(
      map: MapDom[AbsAddr, GaloisIdentity[AbsValue[A, D]]],
      kv: (Loc, AbsValue[A, D])
  ): MapDom[AbsAddr, GaloisIdentity[AbsValue[A, D]]] = {
    val (loc, v) = kv
    loc match {
      case Local(id)     => map.update(AbsAddr(id) -> v)
      case loc: ArrayRef => updateArray(loc, v).rawMap
      case InstanceFieldRef(base, field) =>
        val objRef = get(base)
        objRef match {
          case AbsRef(ids) =>
            ids.foldLeft(map) { (m, i) =>
              val obj = map.get(AbsAddr(i))
              val newObj = obj match {
                case obj: AbsObject[A, D] =>
                  if (ids.size == 1) obj.updateField(field -> v)
                  else obj.weakUpdateField(field           -> v)
                case _ => obj // error case: should be reported
              }
              m.update(AbsAddr(i) -> newObj)
            }
          case _ => map // error case: should be reported
        }
      case StaticFieldRef(clazz, field) =>
        val staticAddr = getStaticAddr(clazz)
        val staticObj  = map.get(staticAddr)
        val newStaticObj = staticObj match {
          case obj: AbsObject[A, D] =>
            obj.weakUpdateField(field -> v)
          case _ =>
            val obj = new AbsObject[A, D]
            obj.updateField(field -> v)
        }
        map.update(staticAddr -> newStaticObj)
      case Param(i) =>
        val addr = getParamAddr(i)
        map.update(addr -> v)
    }
  }
  def update(kv: (Loc, AbsValue[A, D])): This = {
    val newRawMap = updateRawMap(rawMap, kv)
    builder(newRawMap)
  }
  def get(k: Loc): AbsValue[A, D] =
    k match {
      case Local(id)   => rawMap.get(AbsAddr(id))
      case k: ArrayRef => getArray(k)
      case InstanceFieldRef(base, field) =>
        val objRef = get(base)
        objRef match {
          case AbsRef(ids) =>
            ids.foldLeft(AbsBottom: AbsValue[A, D]) { (v, id) =>
              val obj = rawMap.get(AbsAddr(id))
              val fieldValue = obj match {
                case obj: AbsObject[A, D] => obj.getField(field)
                case _                    => AbsBottom
              }
              absValueOps.\/(v, fieldValue)
            }
          case _ => AbsBottom
        }
      case StaticFieldRef(clazz, field) =>
        val staticAddr = getStaticAddr(clazz)
        val obj        = rawMap.get(staticAddr)
        obj match {
          case obj: AbsObject[A, D] => obj.getField(field)
          case _                    => AbsBottom
        }
      case Param(i) =>
        val addr = getParamAddr(i)
        rawMap.get(addr)
    }
  def isStaticAddr(addr: AbsAddr): Boolean =
    addr.id.startsWith(StdObjectModel.staticPrefix)
  def isDynamicAddr(addr: AbsAddr): Boolean =
    addr.id.startsWith(StdObjectModel.dynamicPrefix)

  protected def builder(
      rawMap: MapDom[AbsAddr, GaloisIdentity[AbsValue[A, D]]]
  ): This
}

object StdObjectModel {
  val staticPrefix  = "__static_obj_"
  val dynamicPrefix = "__dynamic_obj_"

  def getStaticAddr(className: ClassName): AbsAddr =
    AbsAddr(s"$staticPrefix${ClassName.toString(className)}")

  var addrIdx         = 0
  var stmtToDyAddrNum = Map.empty[Stmt, Int]
  def getNewAddr(from: Stmt): AbsAddr = {
    val existingIdxOpt = stmtToDyAddrNum.get(from)
    val idx =
      if (existingIdxOpt.isEmpty) {
        addrIdx += 1
        stmtToDyAddrNum += from -> addrIdx
        addrIdx
      } else existingIdxOpt.get
    AbsAddr(s"$dynamicPrefix$idx")
  }
  def getParamAddr(i: Int): AbsAddr = AbsAddr(s"${dynamicPrefix}p$i")

  class AbsObject[A <: Galois: ArithmeticOps, D <: Galois: LatticeWithTopOps]
      extends AbsValue[A, D] {
    implicit val absValueOps = absValueOpsWithObject[A, D]

    protected[mem] var rawFieldMap =
      MapDom.empty[String, GaloisIdentity[AbsValue[A, D]]]

    def \/(that: AbsObject[A, D]): AbsObject[A, D] = {
      val newFieldMap = MapDom
        .ops[String, GaloisIdentity[AbsValue[A, D]]]
        .\/(rawFieldMap, that.rawFieldMap)
      val newObject = new AbsObject[A, D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }

    def partialCompare(that: AbsObject[A, D]): Double = {
      MapDom
        .ops[String, GaloisIdentity[AbsValue[A, D]]]
        .partialCompare(rawFieldMap, that.rawFieldMap)
    }

    def widening(that: AbsObject[A, D])(implicit
        wideningAbsValue: Widening[GaloisIdentity[AbsValue[A, D]]]
    ): AbsObject[A, D] = {
      val newFieldMap = MapDom
        .widening[String, GaloisIdentity[AbsValue[A, D]]]
        .<>(rawFieldMap, that.rawFieldMap)
      val newObject = new AbsObject[A, D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }

    def updateField(kv: (String, AbsValue[A, D])) = {
      val newFieldMap = rawFieldMap.update(kv)
      val newObject   = new AbsObject[A, D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }
    def weakUpdateField(kv: (String, AbsValue[A, D])) = {
      val newFieldMap = rawFieldMap.weakUpdate(kv)
      val newObject   = new AbsObject[A, D]
      newObject.rawFieldMap = newFieldMap
      newObject
    }
    def getField(k: String): AbsValue[A, D] = rawFieldMap.get(k)
  }

  def absValueOpsWithObject[
      A <: Galois: ArithmeticOps,
      D <: Galois: LatticeWithTopOps
  ] =
    new LatticeOps[GaloisIdentity[AbsValue[A, D]]] {
      val absValueOps = AbsValue.ops[A, D]
      override def \/(
          lhs: AbsValue[A, D],
          rhs: AbsValue[A, D]
      ): AbsValue[A, D] = {
        (lhs, rhs) match {
          case (x, y)
              if x.isInstanceOf[AbsObject[A, D]] && y
                .isInstanceOf[AbsObject[A, D]] =>
            x.asInstanceOf[AbsObject[A, D]] \/ y.asInstanceOf[AbsObject[A, D]]
          case (_, _) => absValueOps.\/(lhs, rhs)
        }
      }
      override def partialCompare(
          lhs: AbsValue[A, D],
          rhs: AbsValue[A, D]
      ): Double = {
        (lhs, rhs) match {
          case (x, y)
              if x.isInstanceOf[AbsObject[A, D]] && y
                .isInstanceOf[AbsObject[A, D]] =>
            x.asInstanceOf[AbsObject[A, D]] partialCompare y
              .asInstanceOf[AbsObject[A, D]]
          case (_, _) => absValueOps.partialCompare(lhs, rhs)
        }
      }
      override def bottom: AbsValue[A, D] = absValueOps.bottom
    }

  def wideningWithObject[A <: Galois: Widening, D <: Galois: Widening] =
    new Widening[GaloisIdentity[AbsValue[A, D]]] {
      implicit val wideningAbsValue = AbsValue.widening[A, D]
      override def <>(x: AbsValue[A, D], y: AbsValue[A, D]): AbsValue[A, D] = {
        (x, y) match {
          case (_, _)
              if x.isInstanceOf[AbsObject[A, D]] && y
                .isInstanceOf[AbsObject[A, D]] =>
            x.asInstanceOf[AbsObject[A, D]] widening y
              .asInstanceOf[AbsObject[A, D]]
          case (_, _) => wideningAbsValue.<>(x, y)
        }
      }
    }
}
