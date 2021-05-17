package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.il.CommonIL.MethodSig
import com.simplytyped.yoyak.il.CommonIL.Statement.Invoke

trait Resolvable[D <: MemDomLike[_, _, D]] {
  def resolve(mem: D, invokeStmt: Invoke): List[MethodSig]
}
