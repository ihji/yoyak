package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.CommonIL.Statement.CoreStmt
import com.simplytyped.yoyak.il.cfg.CFG

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
      classes: Map[ClassName, Clazz]
  ) {
    lazy val methods = classes.values.foldLeft(Map.empty[MethodSig, Method]) {
      _ ++ _.methods
    }
    def findByMethodName(name: String): List[Method] = {
      var list = List.empty[Method]
      for ((_, clazz) <- classes; (_, method) <- clazz.methods) {
        if (method.name.methodName == name) list ::= method
      }
      list
    }
  }

  case class Clazz(
      name: ClassName,
      methods: Map[MethodSig, Method],
      interfaces: Set[ClassName],
      superClass: ClassName
  )

  case class Method(
      name: MethodSig,
      statements: List[CoreStmt],
      var cfg: Option[CFG] = None
  )

  case class ClassName(packageName: List[String], name: String)
  object ClassName {
    def apply(className: String): ClassName = {
      val clsName =
        if (className.startsWith("L") && className.endsWith(";")) {
          className.substring(1, className.length - 1).replace("/", ".")
        } else className
      val splitedName = clsName.split('.')
      if (splitedName.isEmpty) ClassName(List.empty, clsName)
      else ClassName(splitedName.dropRight(1).toList, splitedName.last)
    }
    def toString(className: ClassName): String = {
      if (className.packageName.nonEmpty)
        className.packageName.mkString(".") + "." + className.name
      else className.name
    }
  }

  case class MethodSig(
      className: ClassName,
      methodName: String,
      params: List[Type.ValueType]
  )
  object MethodSig {
    val dummy = MethodSig(ClassName("dummy"), "dummy", List())
  }

  object Statement {
    abstract class Stmt extends Attachable {
      override def equals(that: Any): Boolean =
        this eq that.asInstanceOf[AnyRef]
      override def hashCode(): Int = System.identityHashCode(this)

      private[Stmt] def copyAttr(stmt: Stmt): this.type = {
        sourcePos = stmt.pos; this
      }
    }

    case class Block(stmts: StatementContainer) extends Stmt

    case class Switch(v: Value.Loc, keys: List[Value.t], targets: List[Target])
        extends Stmt

    case class Placeholder(x: AnyRef) extends Stmt {
      override def equals(that: Any): Boolean = {
        that match {
          case Placeholder(x2) => x eq x2
          case _               => false
        }
      }
      override def hashCode(): Int = System.identityHashCode(x)
    }

    sealed trait CoreStmt extends Stmt

    case class If(cond: Value.CondBinExp, target: Target) extends CoreStmt

    case class Goto(target: Target) extends CoreStmt

    sealed trait CfgStmt extends CoreStmt

    case class Identity(lv: Value.Local, rv: Value.Param) extends CfgStmt

    case class Assign(lv: Value.Loc, rv: Value.t) extends CfgStmt

    case class Invoke(ret: Option[Value.Local], callee: Type.InvokeType)
        extends CfgStmt

    case class Assume(cond: Value.CondBinExp) extends CfgStmt

    case class Return(v: Option[Value.Loc]) extends CfgStmt

    case class Nop() extends CfgStmt

    case class EnterMonitor(v: Value.Loc) extends CfgStmt

    case class ExitMonitor(v: Value.Loc) extends CfgStmt

    case class Throw(v: Value.Loc) extends CfgStmt

    class Target {
      private var elem: Stmt = _
      def getStmt: Stmt      = elem
      def setStmt(stmt: Stmt): Target = { elem = stmt; this }
      def map(f: Stmt => Stmt): Target = { elem = f(elem); this }
    }

    class StatementContainer {
      private var stmts: List[Stmt] = List.empty
      def getStmts: List[Stmt]      = stmts
      def setStmts(s: List[Stmt]): StatementContainer = { stmts = s; this }
      def map(f: Stmt => Stmt): StatementContainer = {
        stmts = stmts.map { f }; this
      }
    }

    object Stmt {
      object StmtCopier {
        def Block(block: Block, stmts: StatementContainer) =
          new Block(stmts).copyAttr(block)

        def Switch(
            switch: Switch,
            v: Value.Loc,
            keys: List[Value.t],
            targets: List[Target]
        ) =
          new Switch(v, keys, targets).copyAttr(switch)

        def Placeholder(ph: Placeholder, x: AnyRef) =
          new Placeholder(x).copyAttr(ph)

        def If(i: If, cond: Value.CondBinExp, target: Target) =
          new If(cond, target).copyAttr(i)

        def Goto(goto: Goto, target: Target) =
          new Goto(target).copyAttr(goto)

        def Identity(iden: Identity, lv: Value.Local, rv: Value.Param) =
          new Identity(lv, rv).copyAttr(iden)

        def Assign(assign: Assign, lv: Value.Loc, rv: Value.t) =
          new Assign(lv, rv).copyAttr(assign)

        def Invoke(
            invoke: Invoke,
            ret: Option[Value.Local],
            callee: Type.InvokeType
        ) =
          new Invoke(ret, callee).copyAttr(invoke)

        def Assume(assume: Assume, cond: Value.CondBinExp) =
          new Assume(cond).copyAttr(assume)

        def Return(ret: Return, v: Option[Value.Loc]) =
          new Return(v).copyAttr(ret)

        def Nop(nop: Nop) =
          new Nop().copyAttr(nop)

        def EnterMonitor(mon: EnterMonitor, v: Value.Loc) =
          new EnterMonitor(v).copyAttr(mon)

        def ExitMonitor(mon: ExitMonitor, v: Value.Loc) =
          new ExitMonitor(v).copyAttr(mon)

        def Throw(thr: Throw, v: Value.Loc) =
          new Throw(v).copyAttr(thr)
      }
      def expandSwitch(switch: Switch): List[If] = {
        import Value.{CondBinExp, BinOp}
        switch.keys.zip(switch.targets).map { case (k, t) =>
          If(CondBinExp(switch.v, BinOp.==, k), t).copyAttr(switch)
        }
      }
    }
  }

  object Type {
    sealed abstract class InvokeType {
      val callee: MethodSig
      val args: List[Value.Loc]
    }
    case class DynamicInvoke(
        callee: MethodSig,
        args: List[Value.Loc],
        base: Value.Loc
    ) extends InvokeType
    case class StaticInvoke(callee: MethodSig, args: List[Value.Loc])
        extends InvokeType

    sealed abstract class ValueType

    sealed abstract class PrimType extends ValueType
    case object IntegerType        extends PrimType
    case object LongType           extends PrimType
    case object FloatType          extends PrimType
    case object DoubleType         extends PrimType
    case object CharType           extends PrimType
    case object ByteType           extends PrimType
    case object BooleanType        extends PrimType
    case object ShortType          extends PrimType
    case object NullType           extends PrimType
    case object VoidType           extends PrimType

    sealed abstract class ReferenceType          extends ValueType
    case class RefType(className: ClassName)     extends ReferenceType
    case class ArrayType(t: ValueType, dim: Int) extends ReferenceType

    case object UnknownType extends ValueType

    object CommonTypes {
      val String = RefType(ClassName("java.lang.String"))
    }
    def lub(ty1: ValueType, ty2: ValueType): ValueType = {
      (ty1, ty2) match {
        case (IntegerType, IntegerType) => IntegerType
        case (IntegerType, UnknownType) => IntegerType
        case (UnknownType, IntegerType) => IntegerType
        case (LongType, LongType)       => LongType
        case (LongType, UnknownType)    => LongType
        case (UnknownType, LongType)    => LongType
        case (FloatType, FloatType)     => FloatType
        case (FloatType, UnknownType)   => FloatType
        case (UnknownType, FloatType)   => FloatType
        case (DoubleType, DoubleType)   => DoubleType
        case (DoubleType, UnknownType)  => DoubleType
        case (UnknownType, DoubleType)  => DoubleType
        case (CharType, CharType)       => CharType
        case (CharType, UnknownType)    => CharType
        case (UnknownType, CharType)    => CharType
        case (ByteType, ByteType)       => ByteType
        case (ByteType, UnknownType)    => ByteType
        case (UnknownType, ByteType)    => ByteType
        case (BooleanType, BooleanType) => BooleanType
        case (BooleanType, UnknownType) => BooleanType
        case (UnknownType, BooleanType) => BooleanType
        case (ShortType, ShortType)     => ShortType
        case (ShortType, UnknownType)   => ShortType
        case (UnknownType, ShortType)   => ShortType
        case (NullType, x)              => x
        case (x, NullType)              => x
        case (VoidType, VoidType)       => VoidType
        case (VoidType, UnknownType)    => VoidType
        case (UnknownType, VoidType)    => VoidType
        case (ref1 @ RefType(_), ref2 @ RefType(_)) =>
          ref1 // TODO: need to consider class hierarchy
        case (ref @ RefType(_), UnknownType) => ref
        case (UnknownType, ref @ RefType(_)) => ref
        case (arr1 @ ArrayType(_, _), arr2 @ ArrayType(_, _)) =>
          arr1 // TODO: need to consider class hierarchy
        case (arr @ ArrayType(_, _), UnknownType) => arr
        case (UnknownType, arr @ ArrayType(_, _)) => arr
        case _                                    => UnknownType
      }
    }
  }

  object Value {
    sealed abstract class t extends Typable with Attachable {
      private def copyAttr(value: t): this.type = {
        sourcePos = value.pos
        rawType = value.ty
        this
      }
    }

    sealed abstract class Instant extends t

    sealed abstract class Constant extends Instant
    case class IntegerConstant(v: Int) extends Constant {
      override final def ty = Type.IntegerType
    }
    case class LongConstant(v: Long) extends Constant {
      override final def ty = Type.LongType
    }
    case class FloatConstant(v: Float) extends Constant {
      override final def ty = Type.FloatType
    }
    case class DoubleConstant(v: Double) extends Constant {
      override final def ty = Type.DoubleType
    }
    case class CharConstant(v: Char) extends Constant {
      override final def ty = Type.CharType
    }
    case class ByteConstant(v: Byte) extends Constant {
      override final def ty = Type.ByteType
    }
    case class BooleanConstant(v: Boolean) extends Constant {
      override final def ty = Type.BooleanType
    }
    case class ShortConstant(v: Short) extends Constant {
      override final def ty = Type.ShortType
    }
    case class StringConstant(s: String) extends Constant {
      override final def ty = Type.CommonTypes.String
    }
    case class ClassConstant(refTy: Type.ReferenceType) extends Constant
    case object NullConstant                            extends Constant

    sealed abstract class Loc                                  extends Instant
    case class Local(id: String)                               extends Loc
    case class ArrayRef(base: Loc, index: Instant)             extends Loc
    case class InstanceFieldRef(base: Loc, field: String)      extends Loc
    case class StaticFieldRef(clazz: ClassName, field: String) extends Loc
    case class Param(i: Int)                                   extends Loc

    case object This               extends t
    case object CaughtExceptionRef extends t

    case class CastExp(v: Loc, ofTy: Type.ValueType)           extends t
    case class InstanceOfExp(v: Loc, ofTy: Type.ReferenceType) extends t
    case class LengthExp(v: Loc)                               extends t

    case class NewExp(ofTy: Type.RefType)                       extends t
    case class NewArrayExp(ofTy: Type.ValueType, size: Instant) extends t

    sealed abstract class BinExp extends t {
      val lv: Value.t
      val rv: Value.t
      val op: BinOp.Op
    }
    case class CompBinExp(lv: Value.t, op: BinOp.CompOp, rv: Value.t)
        extends BinExp
    case class CondBinExp(lv: Value.t, op: BinOp.CondOp, rv: Value.t)
        extends BinExp {
      def negate: CondBinExp = {
        val newOp = op match {
          case BinOp.<  => BinOp.>=
          case BinOp.<= => BinOp.>
          case BinOp.>  => BinOp.<=
          case BinOp.>= => BinOp.<
          case BinOp.== => BinOp.!=
          case BinOp.!= => BinOp.==
        }
        this.copy(op = newOp)
      }
    }

    object BinOp {
      sealed abstract class Op

      sealed abstract class CondOp extends Op
      case object <                extends CondOp
      case object <=               extends CondOp
      case object >                extends CondOp
      case object >=               extends CondOp
      case object ==               extends CondOp
      case object !=               extends CondOp

      sealed abstract class CompOp extends Op
      case object +                extends CompOp
      case object -                extends CompOp
      case object *                extends CompOp
      case object /                extends CompOp
      case object &&               extends CompOp
      case object ||               extends CompOp
      case object &                extends CompOp
      case object |                extends CompOp
      case object >>               extends CompOp
      case object <<               extends CompOp
      case object >>>              extends CompOp
      case object %                extends CompOp
      case object ^                extends CompOp
      case object cmp              extends CompOp
      case object cmpl             extends CompOp
      case object cmpg             extends CompOp
    }
  }
}
