package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.ArithmeticOps
import com.simplytyped.yoyak.framework.domain.mem.MemElems.AbsValue
import com.simplytyped.yoyak.il.CommonIL.Value.ArrayRef

trait ArrayJoinModel[A,D,This<:ArrayJoinModel[A,D,This]] extends MemDomLike[A,D,This]{
  implicit val arithOps : ArithmeticOps[A]

  def updateArray(kv: (ArrayRef,AbsValue[A,D])) : This = {
    ???
  }
  def getArray(k: ArrayRef) : AbsValue[A,D] = {
    ???
  }
}
