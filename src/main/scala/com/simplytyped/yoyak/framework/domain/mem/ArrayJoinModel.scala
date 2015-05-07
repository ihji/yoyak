package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.{Galois, LatticeWithTopOps, ArithmeticOps}
import com.simplytyped.yoyak.framework.domain.mem.MemElems.AbsValue
import com.simplytyped.yoyak.il.CommonIL.Value.{InstanceFieldRef, ArrayRef}
import ArrayJoinModel._

trait ArrayJoinModel[A<:Galois,D<:Galois,This<:ArrayJoinModel[A,D,This]] extends MemDomLike[A,D,This]{
  implicit val arithOps : ArithmeticOps[A]
  implicit val boxedOps : LatticeWithTopOps[D]

  val absValueOps = AbsValue.ops[A,D]
  protected def updateArray(kv: (ArrayRef,AbsValue[A,D])) : This = {
    val (arrayref,v) = kv
    val dummyRef = InstanceFieldRef(arrayref.base,arrayFieldName)
    val orig = get(dummyRef)
    update(dummyRef->AbsValue.ops[A,D].\/(orig,v))
  }
  protected def getArray(k: ArrayRef) : AbsValue[A,D] = {
    get(InstanceFieldRef(k.base,arrayFieldName))
  }
}

object ArrayJoinModel {
  val arrayFieldName = "__joined_array_elems"
}
