package com.simplytyped.yoyak.framework.semantics

import com.simplytyped.yoyak.framework.domain.Galois
import com.simplytyped.yoyak.il.CommonIL.Statement._

trait AbstractTransferable[D<:Galois] {
  def transfer(input: D#Abst, stmt: CoreStmt) : D#Abst = {
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
  protected def transferIdentity(stmt: Identity, input: D#Abst) : D#Abst = input
  protected def transferAssign(stmt: Assign, input: D#Abst) : D#Abst = input
  protected def transferInvoke(stmt: Invoke, input: D#Abst) : D#Abst = input
  protected def transferIf(stmt: If, input: D#Abst) : D#Abst = input
  protected def transferAssume(stmt: Assume, input: D#Abst) : D#Abst = input
  protected def transferReturn(stmt: Return, input: D#Abst) : D#Abst = input
  protected def transferNop(stmt: Nop, input: D#Abst) : D#Abst = input
  protected def transferGoto(stmt: Goto, input: D#Abst) : D#Abst = input
  protected def transferEnterMonitor(stmt: EnterMonitor, input: D#Abst) : D#Abst = input
  protected def transferExitMonitor(stmt: ExitMonitor, input: D#Abst) : D#Abst = input
  protected def transferThrow(stmt: Throw, input: D#Abst) : D#Abst = input
}
