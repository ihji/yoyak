package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.ArithmeticOps
import com.simplytyped.yoyak.framework.domain.mem.MemElems.AbsValue
import com.simplytyped.yoyak.il.CommonIL.Value.{InstanceFieldRef, ArrayRef}
import ArrayJoinModel._

trait ArrayJoinModel[A,D,This<:ArrayJoinModel[A,D,This]] extends MemDomLike[A,D,This]{
  implicit val arithOps : ArithmeticOps[A]

  def updateArray(kv: (ArrayRef,AbsValue[A,D])) : This = {
    val (arrayref,v) = kv
    update(InstanceFieldRef(arrayref.base,arrayFieldName)->v)
  }
  def getArray(k: ArrayRef) : AbsValue[A,D] = {
    get(InstanceFieldRef(k.base,arrayFieldName))
  }
}

object ArrayJoinModel {
  val arrayFieldName = "__joined_array_elems"
}
