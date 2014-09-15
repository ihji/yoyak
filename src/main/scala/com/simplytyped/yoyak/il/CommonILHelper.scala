package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Type._

object CommonILHelper {
  def mergeStmts(stmts1: List[Stmt], stmt2: List[Stmt]) : List[Stmt] = {
    insertStmts(stmts1,stmts1.length,stmt2)
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
  def insertStmts(dest: List[Stmt], pos: Int, src: List[Stmt]) : List[Stmt] = {
    val range = pos until dest.length
    val destShift = src.length
    val shiftF = (x: Int) => {if(range.contains(x)) x + destShift else x}
    val shiftedDest = dest.map{
      case s@Switch(_,_,offsets) => s.copy(offsets = offsets.map{shiftF})
      case i@If(_,offset) => i.copy(thenOffset = shiftF(offset))
      case g@Goto(offset) => g.copy(jumpOffset = shiftF(offset))
      case x => x
    }

    val srcShift = pos
    val shiftedSrc = src.map{
      case s@Switch(_,_,offsets) => s.copy(offsets = offsets.map{_+srcShift})
      case i@If(_,offset) => i.copy(thenOffset = offset+srcShift)
      case g@Goto(offset) => g.copy(jumpOffset = offset+srcShift)
      case x => x
    }
    val (before,after) = shiftedDest.splitAt(pos)
    before++shiftedSrc++after
  }
}
