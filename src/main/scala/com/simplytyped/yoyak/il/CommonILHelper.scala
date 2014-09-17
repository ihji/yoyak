package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.CommonIL.Statement.Stmt.StmtCopier
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
      else stmt match {
        case block@Block(innerStmts) =>
          val translated = stmtSubstitute(map)(innerStmts)
          StmtCopier.Block(block, translated)
        case switch@Switch(v, keys, targets) =>
          val translated = stmtSubstitute(map)(targets)
          StmtCopier.Switch(switch, v, keys, translated)
        case iff@If(cond, target) =>
          val translated = substitute(target)
          StmtCopier.If(iff,cond,translated)
        case goto@Goto(target) =>
          val translated = substitute(target)
          StmtCopier.Goto(goto,translated)
        case _ => stmt
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
        val output = expandStmts(innerStmts)
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
