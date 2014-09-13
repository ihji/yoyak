package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.CommonIL.Statement.Stmt

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
  case class Program(
    classes : Map[ClassName,Clazz]
  )

  case class Clazz(
    name : ClassName,
    methods : Map[MethodSig,Method],
    interfaces : Set[ClassName],
    superClass : ClassName
  )

  case class Method(
    name : MethodSig,
    statements : List[Stmt]
  )

  case class ClassName(packageName: List[String], name: String)
  object ClassName {
    def apply(className: String) : ClassName = {
      val splitedName = className.split('.')
      if(splitedName.isEmpty) ClassName(List.empty,className)
      else ClassName(splitedName.dropRight(1).toList,splitedName.last)
    }
  }

  case class MethodSig(className: ClassName, methodName: String, params: List[Type.ValueType])
  object MethodSig {
    val dummy = MethodSig(ClassName("dummy"), "dummy", List())
  }

  object Statement {
    sealed abstract class Stmt extends Attachable

    case class Identity(lv: Value.Local, rv: Value.Param) extends Stmt

    case class Assign(lv: Value.Loc, rv: Value.t) extends Stmt

    case class Invoke(ret: Option[Value.Loc], callee: Type.InvokeType) extends Stmt

    case class If(cond: Value.CondBinExp, thenOffset: Int) extends Stmt

    case class Switch(v: Value.Loc, keys: List[Value.t], offsets: List[Int]) extends Stmt

    case class Return(v: Option[Value.t]) extends Stmt

    case class Nop() extends Stmt

    case class Goto(jumpOffset: Int) extends Stmt

    case class EnterMonitor(v: Value.Loc) extends Stmt

    case class ExitMonitor(v: Value.Loc) extends Stmt

    case class Throw(v: Value.Loc) extends Stmt
  }

  object Type {
    sealed abstract class InvokeType {
      val callee: MethodSig
      val args: List[Value.t]
    }
    case class DynamicInvoke(callee: MethodSig, args: List[Value.t], base: Value.Loc) extends InvokeType
    case class StaticInvoke(callee: MethodSig, args: List[Value.t]) extends InvokeType

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
    case object VoidType extends PrimType

    case class RefType(className: ClassName) extends ValueType
    case class ArrayType(t: ValueType, dim: Int) extends ValueType
    case object UnknownType extends ValueType
  }

  object Value {
    sealed abstract class t {
      private[this] var rawTy : Type.ValueType = Type.UnknownType
      def setType(ty: Type.ValueType) : this.type = { rawTy = ty; this }
      def `type` = rawTy
    }

    case class IntegerConstant(v: Int) extends t
    case class LongConstant(v: Long) extends t
    case class FloatConstant(v: Float) extends t
    case class DoubleConstant(v: Double) extends t
    case class CharConstant(v: Char) extends t
    case class ByteConstant(v: Byte) extends t
    case class BooleanConstant(v: Boolean) extends t
    case class ShortConstant(v: Short) extends t
    case class StringConstant(s: String) extends t
    case class ClassConstant(ty: Type.ValueType) extends t
    case object NullConstant extends t

    case object This extends t
    case object CaughtExceptionRef extends t
    case class Param(i: Int) extends t

    case class CastExp(v: Loc, ty: Type.ValueType) extends t
    case class InstanceOfExp(v: Loc, ty: Type.ValueType) extends t
    case class LengthExp(v: Loc) extends t

    case class NewExp(ty: Type.ValueType) extends t
    case class NewArrayExp(ty: Type.ValueType, size: Loc) extends t

    sealed abstract class BinExp extends t {
      val lv : Value.t
      val rv : Value.t
      val op : BinOp.Op
    }
    case class CompBinExp(lv: Value.t, op: BinOp.CompOp, rv: Value.t) extends BinExp
    case class CondBinExp(lv: Value.t, op: BinOp.CondOp, rv: Value.t) extends BinExp

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
      case object cmp extends CompOp
      case object cmpl extends CompOp
      case object cmpg extends CompOp
    }

    sealed abstract class Loc extends t
    case class Local(id: String, ty: Type.ValueType) extends Loc
    case class ArrayRef(base: Loc, index: Loc) extends Loc
    case class InstanceFieldRef(base: Loc, field: String) extends Loc
    case class StaticFieldRef(clazz: ClassName, field: String) extends Loc
  }
}
