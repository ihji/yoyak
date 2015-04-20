package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.NodeLike
import com.simplytyped.yoyak.il.CommonIL.MethodSig
import com.simplytyped.yoyak.il.CommonIL.Statement.{Nop, CoreStmt}
import com.simplytyped.yoyak.il.cfg.BasicBlock.{ExitMarkerContainer, EntryMarkerContainer, AbstractStatementContainer}

case class BasicBlock(data: AbstractStatementContainer, id: Int) extends NodeLike[BasicBlock] {
  type D = AbstractStatementContainer
  override def toString = s"BasicBlock($id)"
  def isEntry : Boolean = data == EntryMarkerContainer
  def isExit : Boolean = data == ExitMarkerContainer
}

object BasicBlock {
  private var idCounter = 0
  private var idToMethodSigMap = Map.empty[Int,MethodSig]
  abstract class AbstractStatementContainer {
    def getStmts : List[CoreStmt]
    def setStmts(s : List[CoreStmt]) : AbstractStatementContainer
  }
  object EntryMarkerContainer extends AbstractStatementContainer {
    def getStmts : List[CoreStmt] = List(Nop())
    def setStmts(s : List[CoreStmt]) : AbstractStatementContainer = this
  }
  object ExitMarkerContainer extends AbstractStatementContainer {
    def getStmts : List[CoreStmt] = List(Nop())
    def setStmts(s : List[CoreStmt]) : AbstractStatementContainer = this
  }
  class CoreStatementContainer extends AbstractStatementContainer {
    private var stmts : List[CoreStmt] = List.empty
    def getStmts = stmts
    def setStmts(s : List[CoreStmt]) = {stmts = s; this}
  }
  def apply(methodSig: MethodSig)(stmts: List[CoreStmt]) : BasicBlock = {
    idCounter += 1
    idToMethodSigMap += idCounter -> methodSig
    BasicBlock(new CoreStatementContainer().setStmts(stmts), idCounter)
  }
  def getEntryBlock() : BasicBlock = {
    idCounter += 1
    BasicBlock(EntryMarkerContainer,idCounter)
  }
  def getExitBlock() : BasicBlock = {
    idCounter += 1
    BasicBlock(ExitMarkerContainer,idCounter)
  }
  def getMethodSig(bb: BasicBlock) : Option[MethodSig] = idToMethodSigMap.get(bb.id)
}