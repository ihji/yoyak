package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.mem.MemElems.{AbsAddr, AbsValue}
import com.simplytyped.yoyak.il.CommonIL.Value.{Local, Loc}

trait MemDomLike[A,D,This<:MemDomLike[A,D,This]] {
  def update(kv: (Loc,AbsValue[A,D])) : This
  def remove(loc: Local) : This
  def alloc(loc: Loc) : This
  def get(k: Loc) : AbsValue[A,D]
  def isStaticAddr(addr: AbsAddr) : Boolean
  def isDynamicAddr(addr: AbsAddr) : Boolean
}
