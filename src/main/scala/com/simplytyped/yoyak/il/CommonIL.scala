package com.simplytyped.yoyak.il

/* container for common IL
   common IL is an input program representation before graph transformation
   - assign statement
   - invoke statement
   - if statement
   - block statement
   - return statement
   - nop statement
   - goto statement
*/
object CommonIL {

  case class MethodSig(className: String, methodName: String, params: List[Type.ValueType])

  object Statement {
    sealed abstract class Stmt {
      val sourceInfo : SourceInfo
    }

    case class Assign(lv: Value.Loc, rv: Value.Exp, sourceInfo: SourceInfo) extends Stmt

    case class Invoke(ret: Option[Value.Loc], callee: Type.InvokeType, sourceInfo: SourceInfo) extends Stmt

    case class If(cond: Value.CondBinExp, thenBlock: Block, elseBlock: Block, sourceInfo: SourceInfo) extends Stmt

    case class Block(stmts: List[Stmt], sourceInfo: SourceInfo) extends Stmt

    case class Return(v: Option[Value.Exp], sourceInfo: SourceInfo) extends Stmt

    case class Nop(sourceInfo: SourceInfo) extends Stmt

    case class Goto(jumpTo: Stmt, sourceInfo: SourceInfo) extends Stmt
  }

  object Type {
    sealed abstract class InvokeType {
      val callee: MethodSig
      val args: List[Value.Exp]
    }
    case class DynamicInvoke(callee: MethodSig, args: List[Value.Exp], base: Value.Loc) extends InvokeType
    case class StaticInvoke(callee: MethodSig, args: List[Value.Exp]) extends InvokeType

    sealed abstract class ValueType
    sealed abstract class PrimType extends ValueType
    case object IntegerType extends PrimType
    case object LongType extends PrimType
    case object FloatType extends PrimType
    case object DoubleType extends PrimType
    case object CharType extends PrimType
    case object ByteType extends PrimType
    case object BooleanType extends PrimType
    case object ShortType extends PrimType
    case object NullType extends PrimType

    case class RefType(className: String) extends ValueType
    case class ArrayType(t: ValueType, dim: Int) extends ValueType
    case object UnknownType extends ValueType
  }

  object Value {
    sealed abstract class Exp
    case class IntegerConstant(v: Int) extends Exp
    case class LongConstant(v: Long) extends Exp
    case class FloatConstant(v: Float) extends Exp
    case class DoubleConstant(v: Double) extends Exp
    case class CharConstant(v: Char) extends Exp
    case class ByteConstant(v: Byte) extends Exp
    case class BooleanConstant(v: Boolean) extends Exp
    case class ShortConstant(v: Short) extends Exp
    case object NullConstant extends Exp

    sealed abstract class BinExp extends Exp {
      val lv : Value.Exp
      val rv : Value.Exp
      val op : BinOp.Op
    }
    case class CompBinExp(lv: Value.Exp, op: BinOp.CompOp, rv: Value.Exp) extends BinExp
    case class CondBinExp(lv: Value.Exp, op: BinOp.CondOp, rv: Value.Exp) extends BinExp

    object BinOp {
      sealed abstract class Op

      sealed abstract class CondOp extends Op
      case object < extends CondOp
      case object <= extends CondOp
      case object > extends CondOp
      case object >= extends CondOp
      case object == extends CondOp
      case object != extends CondOp

      sealed abstract class CompOp extends Op
      case object + extends CompOp
      case object - extends CompOp
      case object * extends CompOp
      case object / extends CompOp
      case object && extends CompOp
      case object || extends CompOp
      case object & extends CompOp
      case object | extends CompOp
      case object >> extends CompOp
      case object << extends CompOp
      case object >>> extends CompOp
      case object % extends CompOp
      case object ^ extends CompOp
    }

    sealed abstract class Loc
  }
}
