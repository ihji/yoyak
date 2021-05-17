package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.il.CommonIL.Value.Local

trait Localizable[D <: MemDomLike[_, _, D]] {
  val returningPlaceholder: Local
  def deleteLocals(input: D): D
}
