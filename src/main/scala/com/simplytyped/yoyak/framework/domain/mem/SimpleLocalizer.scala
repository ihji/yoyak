package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.{Galois, LatticeWithTopOps, ArithmeticOps}
import com.simplytyped.yoyak.framework.domain.mem.MemElems.{AbsValue, AbsRef, AbsAddr}
import com.simplytyped.yoyak.il.CommonIL.Value.Local

class SimpleLocalizer[A <: Galois : ArithmeticOps, D <: Galois : LatticeWithTopOps] extends Localizable[MemDom[A,D]] {
  val returningPlaceholder = Local("$__ret")
  def deleteLocals(input: MemDom[A,D]) : MemDom[A,D] = {
    val safeAddrs = input.rawMap.foldLeft(Set.empty[AbsAddr]) {
      case (s, (a,_)) =>
        a match {
          case _ if input.isStaticAddr(a) => s + a
          case _ if a.id == returningPlaceholder.id => s + a
          case _ => s
        }
    }
    moveObjects(input, safeAddrs)
  }
  private def moveObjects(oldMem: MemDom[A,D], targets: Set[AbsAddr], newMem: MemDom[A,D] = MemDom.empty) : MemDom[A,D] = {
    if(targets.isEmpty) newMem
    else {
      val (newMemory, newTargets) = targets.foldLeft(newMem, Set.empty[AbsAddr]) {
        case ((m, s), a) =>
          val value = oldMem.rawMap.get(a)
          val ns = getAddrsFromValue(value)
          val nm = MemDom.empty[A,D]
          nm.rawMap = m.rawMap.update(a,value)
          (nm,ns)
      }
      moveObjects(oldMem, newTargets, newMemory)
    }
  }
  private def getAddrsFromValue(v: AbsValue[A,D]) : Set[AbsAddr] = {
    v match {
      case obj : MemDom[A,D]#AbsObject =>
        obj.rawFieldMap.foldLeft(Set.empty[AbsAddr]) {
          case (s,(_,fieldValue)) => s ++ getAddrsFromValue(fieldValue)
        }
      case AbsRef(ids) => ids.map{AbsAddr}
      case _ => Set.empty
    }
  }
}
