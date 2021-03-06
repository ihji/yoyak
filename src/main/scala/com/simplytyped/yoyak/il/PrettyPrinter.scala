package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.CommonIL._
import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Value._
import com.simplytyped.yoyak.il.CommonIL.Type._
import com.simplytyped.yoyak.il.cfg.BasicBlock.{
  ExitMarkerContainer,
  EntryMarkerContainer,
  CoreStatementContainer
}
import com.simplytyped.yoyak.il.cfg.{BasicEdge, BasicBlock, CFG}

class PrettyPrinter {
  def toDot(cfg: CFG): String = {
    val buf = new StringBuffer()
    buf.append("digraph yoyak {\n")
    buf.append(cfg.nodes.map { toString }.mkString(";\n"))
    buf.append(";\n")
    buf.append(cfg.edges.map { toString }.mkString(";\n"))
    buf.append(";\n}")
    buf.toString
  }
  def toString(node: BasicBlock): String = {
    node.data match {
      case _: CoreStatementContainer =>
        "\"" + node.id + ":\\n" + node.data.getStmts
          .map { toString }
          .map { _.replace("\"", "'") }
          .mkString("\\n") + "\""
      case EntryMarkerContainer => "\"ENTRY\""
      case ExitMarkerContainer  => "\"EXIT\""
    }

  }
  def toString(edge: BasicEdge): String = {
    s"${toString(edge.from)} -> ${toString(edge.to)}"
  }
  def toString(pgm: Program): String = {
    pgm.classes.map { c => toString(c._2) }.mkString("\n\n")
  }
  def toString(clazz: Clazz): String = {
    clazz.methods.map { m => toString(m._2) }.mkString("\n\n")
  }
  def toString(method: Method): String = {
    s"${toString(method.name)}\n${method.statements.zipWithIndex
      .map { s => s"${s._2} ${toString(s._1)}" }
      .mkString("\n")}"
  }
  def toString(sig: MethodSig): String = {
    s"${ClassName.toString(sig.className)}.${sig.methodName}(${sig.params.map { toString }.mkString(",")})"
  }
  def toString(stmt: Stmt): String = {
    stmt match {
      case Block(stmts) => stmts.getStmts.map { toString }.mkString("\n")

      case Switch(v, keys, targets) =>
        s"switch(${toString(v)})\n${keys
          .zip(targets)
          .map { case (k, goto) => s"case ${toString(k)} => goto $goto" }
          .mkString("\n")}"

      case Identity(lv, rv) => s"${toString(lv)} := ${toString(rv)}"

      case Assign(lv, rv) => s"${toString(lv)} = ${toString(rv)}"

      case Invoke(ret, callee) =>
        callee match {
          case DynamicInvoke(sig, args, base) =>
            s"${ret.map { x => s"${toString(x)} = " }.getOrElse("")}${toString(
              base
            )}.${toString(sig)}(${args.map { toString }.mkString(",")})"
          case StaticInvoke(sig, args) =>
            s"${ret.map { x => s"${toString(x)} = " }.getOrElse("")}${toString(
              sig
            )}(${args.map { toString }.mkString(",")})"
        }

      case If(cond, target) => s"if(${toString(cond)}) goto $target"

      case Assume(cond) => s"assume(${toString(cond)})"

      case Return(v) => s"return ${v.map { toString }.getOrElse("")}"

      case Nop() => "nop"

      case Goto(target) => s"goto $target"

      case EnterMonitor(v) => s"enterMonitor(${toString(v)})"

      case ExitMonitor(v) => s"exitMonitor(${toString(v)})"

      case Throw(v) => s"throw ${toString(v)}"
    }
  }
  def toString(va: Value.t): String = {
    va match {
      case IntegerConstant(v)   => v.toString
      case LongConstant(v)      => v.toString
      case FloatConstant(v)     => v.toString
      case DoubleConstant(v)    => v.toString
      case CharConstant(v)      => v.toString
      case ByteConstant(v)      => v.toString
      case BooleanConstant(v)   => v.toString
      case ShortConstant(v)     => v.toString
      case StringConstant(s)    => "\"" + s + "\""
      case ClassConstant(refTy) => s"${toString(refTy)}.class"
      case NullConstant         => "null"

      case Local(id)                     => s"$id:${toString(va.ty)}"
      case ArrayRef(base, index)         => s"${toString(base)}[${toString(index)}]"
      case InstanceFieldRef(base, field) => s"${toString(base)}.$field"
      case StaticFieldRef(clazz, field) =>
        s"${ClassName.toString(clazz)}.$field"

      case This               => "$this"
      case CaughtExceptionRef => "$exception"
      case Param(i)           => "$param" + i

      case CastExp(v, ty)       => s"(${toString(ty)}) ${toString(v)}"
      case InstanceOfExp(v, ty) => s"${toString(v)} instanceof ${toString(ty)}"
      case LengthExp(v)         => s"lengthof ${toString(v)}"

      case NewExp(ty)            => s"new ${toString(ty)}"
      case NewArrayExp(ty, size) => s"new ${toString(ty)}[${toString(size)}]"

      case CompBinExp(lv, op, rv) =>
        s"${toString(lv)} ${toString(op)} ${toString(rv)}"
      case CondBinExp(lv, op, rv) =>
        s"${toString(lv)} ${toString(op)} ${toString(rv)}"
    }
  }
  def toString(op: BinOp.Op): String = op.toString
  def toString(ty: Type.ValueType): String = {
    ty match {
      case IntegerType => "int"
      case LongType    => "long"
      case FloatType   => "float"
      case DoubleType  => "double"
      case CharType    => "char"
      case ByteType    => "byte"
      case BooleanType => "boolean"
      case ShortType   => "short"
      case NullType    => "null"
      case VoidType    => "void"

      case RefType(className: ClassName)     => ClassName.toString(className)
      case ArrayType(t: ValueType, dim: Int) => s"${toString(t)}[$dim]"
      case UnknownType                       => "unknown"
    }
  }
}

object PrettyPrinter {
  def print(pgm: Program) = {
    val printer = new PrettyPrinter
    println(printer.toString(pgm))
  }
  def printByMethodName(name: String, pgm: Program) = {
    val printer = new PrettyPrinter
    pgm.findByMethodName(name).foreach { m => println(printer.toString(m)) }
  }
}
