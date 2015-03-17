package com.simplytyped.yoyak.framework.domain.mem

trait Localizable[D<:MemDomLike[_,_,D]] {
  def deleteLocals(input: D) : D
}
