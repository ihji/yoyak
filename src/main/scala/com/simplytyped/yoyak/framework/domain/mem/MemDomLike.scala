package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.mem.MemElems.AbsValue
import com.simplytyped.yoyak.il.CommonIL.Value.Loc

trait MemDomLike[A,D,This<:MemDomLike[A,D,This]] {
  def update(kv: (Loc,AbsValue[A,D])) : This
  def alloc(loc: Loc) : This
  def get(k: Loc) : AbsValue[A,D]
}
