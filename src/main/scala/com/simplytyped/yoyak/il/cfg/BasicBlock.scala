package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.NodeLike
import com.simplytyped.yoyak.il.CommonIL.Statement.CoreStmt
import com.simplytyped.yoyak.il.cfg.BasicBlock.CoreStatementContainer

case class BasicBlock(data: CoreStatementContainer, id: Int) extends NodeLike[BasicBlock] {
  type D = CoreStatementContainer
  override def toString = s"BasicBlock($id)"
}

object BasicBlock {
  var idCounter = 0
  class CoreStatementContainer {
    private var stmts : List[CoreStmt] = List.empty
    def getStmts : List[CoreStmt] = stmts
    private[il] def setStmts(s : List[CoreStmt]) : CoreStatementContainer = {stmts = s; this}
  }
  def apply(stmts: List[CoreStmt]) : BasicBlock = {
    idCounter += 1
    BasicBlock(new CoreStatementContainer().setStmts(stmts), idCounter)
  }
}