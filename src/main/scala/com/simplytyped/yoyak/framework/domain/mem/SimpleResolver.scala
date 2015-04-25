package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.il.CommonIL.MethodSig
import com.simplytyped.yoyak.il.CommonIL.Statement.Invoke
import com.simplytyped.yoyak.il.CommonIL.Type.{StaticInvoke, DynamicInvoke}

class SimpleResolver[A,D] extends Resolvable[MemDom[A,D]] {
  def resolve(mem: MemDom[A,D], invokeStmt: Invoke) : List[MethodSig] = {
    invokeStmt match {
      case Invoke(_,DynamicInvoke(callee, args, base)) => List(callee)
      case Invoke(_,StaticInvoke(callee, args)) => List(callee)
    }
  }
}
