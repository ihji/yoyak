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
  protected def transferIdentity(stmt: Identity, input: D) : D
  protected def transferAssign(stmt: Assign, input: D) : D
  protected def transferInvoke(stmt: Invoke, input: D) : D
  protected def transferIf(stmt: If, input: D) : D
  protected def transferAssume(stmt: Assume, input: D) : D
  protected def transferReturn(stmt: Return, input: D) : D
  protected def transferNop(stmt: Nop, input: D) : D
  protected def transferGoto(stmt: Goto, input: D) : D
  protected def transferEnterMonitor(stmt: EnterMonitor, input: D) : D
  protected def transferExitMonitor(stmt: ExitMonitor, input: D) : D
  protected def transferThrow(stmt: Throw, input: D) : D
}
