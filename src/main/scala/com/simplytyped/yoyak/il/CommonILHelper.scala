package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.CommonIL.Statement.{Goto, Switch, If, Stmt}
import com.simplytyped.yoyak.il.CommonIL.Type._

object CommonILHelper {
  def mergeStmts(stmts1: List[Stmt], stmt2: List[Stmt]) : List[Stmt] = {
    val shift = stmts1.length
    val shiftedStmt2 = stmt2.map{
      case s@Switch(_,_,offsets) => s.copy(offsets = offsets.map{_+shift})
      case i@If(_,offset) => i.copy(thenOffset = offset+shift)
      case g@Goto(offset) => g.copy(jumpOffset = offset+shift)
      case x => x
    }
    stmts1 ++ shiftedStmt2
  }
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
}
