package com.simplytyped.yoyak.semantics

import com.simplytyped.yoyak.il.CommonIL.Statement._

trait AbstractTransferable[D] {
  def transfer(stmt: Stmt, dom: D) : D = {
    stmt match {
      case s: Identity => transferIdentity(s, dom)
      case s: Assign => transferAssign(s, dom)
      case s: Invoke => transferInvoke(s, dom)
      case s: If => transferIf(s, dom)
      case s: Assume => transferAssume(s, dom)
      case s: Return => transferReturn(s, dom)
      case s: Nop => transferNop(s, dom)
      case s: Goto => transferGoto(s, dom)
      case s: EnterMonitor => transferEnterMonitor(s, dom)
      case s: ExitMonitor => transferExitMonitor(s, dom)
      case s: Throw => transferThrow(s, dom)
    }
  }
  def transferIdentity(stmt: Identity, dom: D) : D
  def transferAssign(stmt: Assign, dom: D) : D
  def transferInvoke(stmt: Invoke, dom: D) : D
  def transferIf(stmt: If, dom: D) : D
  def transferAssume(stmt: Assume, dom: D) : D
  def transferReturn(stmt: Return, dom: D) : D
  def transferNop(stmt: Nop, dom: D) : D
  def transferGoto(stmt: Goto, dom: D) : D
  def transferEnterMonitor(stmt: EnterMonitor, dom: D) : D
  def transferExitMonitor(stmt: ExitMonitor, dom: D) : D
  def transferThrow(stmt: Throw, dom: D) : D
}
