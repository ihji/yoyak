package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Type._

object CommonILHelper {
  def getUnitSizeOf(ty: ValueType) : Int = {
    ty match {
      case IntegerType => 1
      case LongType => 2
      case FloatType => 1
      case DoubleType => 2
      case CharType => 1
      case ByteType => 1
      case BooleanType => 1
      case ShortType => 1
      case NullType => 1
      case VoidType => 1

      case _ : RefType => 1
      case _ : ArrayType => 1
      case UnknownType => 1
    }
  }
  def stmtSubstitute(map: Map[Stmt,Stmt])(stmts: List[Stmt]) : List[Stmt] = {
    def substitute(stmt: Stmt) : Stmt = {
      val srcOpt = map.get(stmt)
      if(srcOpt.nonEmpty) srcOpt.get
      else {
        stmt match {
          case block : Block =>
            block.stmts.map{substitute}
          case switch : Switch =>
            switch.targets.map{_.map{substitute}}
          case iff : If =>
            iff.target.map{substitute}
          case goto : Goto =>
            goto.target.map{substitute}
          case _ => // do nothing
        }
        stmt
      }
    }
    stmts.map{substitute}
  }
  def stmtSubstituteCore(map: Map[Stmt,CoreStmt])(stmts: List[CoreStmt]) : List[CoreStmt] = {
    stmtSubstitute(map)(stmts).asInstanceOf[List[CoreStmt]]
  }
  def expandStmts(stmts: List[Stmt]) : List[CoreStmt] = {
    var map = Map.empty[Stmt,CoreStmt]
    val resultStmts = stmts.flatMap {
      case block@Block(innerStmts) =>
        val output = expandStmts(innerStmts.getStmts)
        map += (block->output.head)
        output
      case switch : Switch =>
        val output = Stmt.expandSwitch(switch)
        map += (switch->output.head)
        output
      case x : CoreStmt => List(x)
    }
    stmtSubstituteCore(map)(resultStmts)
  }
}
