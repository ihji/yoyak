package com.simplytyped.yoyak.framework.semantics

import com.simplytyped.yoyak.il.CommonIL.Statement._

trait AbstractTransferable[D] {
  def transfer(input: D, stmt: CoreStmt) : D = {
    stmt match {
      case s: Identity => transferIdentity(s, input)
      case s: Assign => transferAssign(s, input)
      case s: Invoke => transferInvoke(s, input)
      case s: If => transferIf(s, input)
      case s: Assume => transferAssume(s, input)
      case s: Return => transferReturn(s, input)
      case s: Nop => transferNop(s, input)
      case s: Goto => transferGoto(s, input)
      case s: EnterMonitor => transferEnterMonitor(s, input)
      case s: ExitMonitor => transferExitMonitor(s, input)
      case s: Throw => transferThrow(s, input)
    }
  }
  protected def transferIdentity(stmt: Identity, input: D) : D = input
  protected def transferAssign(stmt: Assign, input: D) : D = input
  protected def transferInvoke(stmt: Invoke, input: D) : D = input
  protected def transferIf(stmt: If, input: D) : D = input
  protected def transferAssume(stmt: Assume, input: D) : D = input
  protected def transferReturn(stmt: Return, input: D) : D = input
  protected def transferNop(stmt: Nop, input: D) : D = input
  protected def transferGoto(stmt: Goto, input: D) : D = input
  protected def transferEnterMonitor(stmt: EnterMonitor, input: D) : D = input
  protected def transferExitMonitor(stmt: ExitMonitor, input: D) : D = input
  protected def transferThrow(stmt: Throw, input: D) : D = input
}
