package com.simplytyped.yoyak.semantics

import com.simplytyped.yoyak.il.CommonIL.Statement._

trait AbstractTransferable[D] {
  def transfer(stmt: Stmt, dom: D) : D = {
    stmt match {
      case s: Identity => transferIdentity(s, dom)
      case s: Assign => transferAssign(s, dom)
      case s: Invoke => transferInvoke(s, dom)
      case s: If => transferIf(s, dom)
      case s: Block => transferBlock(s, dom)
      case s: Return => transferReturn(s, dom)
      case s: Nop => transferNop(s, dom)
      case s: Goto => transferGoto(s, dom)
    }
  }
  def transferIdentity(stmt: Identity, dom: D) : D
  def transferAssign(stmt: Assign, dom: D) : D
  def transferInvoke(stmt: Invoke, dom: D) : D
  def transferIf(stmt: If, dom: D) : D
  def transferBlock(stmt: Block, dom: D) : D
  def transferReturn(stmt: Return, dom: D) : D
  def transferNop(stmt: Nop, dom: D) : D
  def transferGoto(stmt: Goto, dom: D) : D
}
