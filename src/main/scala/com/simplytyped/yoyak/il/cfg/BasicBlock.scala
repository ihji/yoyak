package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.NodeLike
import com.simplytyped.yoyak.il.CommonIL.Statement.CoreStmt
import com.simplytyped.yoyak.il.cfg.BasicBlock.CoreStatementContainer

case class BasicBlock(data: CoreStatementContainer) extends NodeLike[BasicBlock] {
  type D = CoreStatementContainer
}

object BasicBlock {
  class CoreStatementContainer {
    private var stmts : List[CoreStmt] = List.empty
    def getStmts : List[CoreStmt] = stmts
    def setStmts(s : List[CoreStmt]) : CoreStatementContainer = {stmts = s; this}
  }
  def apply(stmts: List[CoreStmt]) : BasicBlock = BasicBlock(new CoreStatementContainer().setStmts(stmts))
}