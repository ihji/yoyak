package com.simplytyped.yoyak.parser.cil

import com.simplytyped.yoyak.il.CommonIL._
import com.simplytyped.yoyak.parser.cil.{CommonILParser => CIL}

class CommonILTransform {
  def transformType(ty: CIL.Type) : Type.ValueType = {
    ty match {
      case CIL.IntegerType => Type.IntegerType
      case CIL.StringType => Type.CommonTypes.String
    }
  }
  def transformValue(vl: CIL.Value) : Value.t = {
    vl match {
      case CIL.Ident(id) => Value.Local(id)
      case CIL.CInteger(v) => Value.IntegerConstant(v)
      case CIL.CString(v) => Value.StringConstant(v)
      case exp : CIL.BinExp => transformExp(exp)
    }
  }
  def transformExp(exp: CIL.BinExp) : Value.BinExp = {
    val lv = transformValue(exp.lv)
    val rv = transformValue(exp.rv)
    val op = transformOp(exp.op)
    op match {
      case o : Value.BinOp.CompOp => Value.CompBinExp(lv,o,rv)
      case o : Value.BinOp.CondOp => Value.CondBinExp(lv,o,rv)
    }
  }
  def transformOp(op: CIL.Operator) : Value.BinOp.Op = {
    op match {
      case CIL.Add => Value.BinOp.+
      case CIL.Sub => Value.BinOp.-
      case CIL.Mul => Value.BinOp.*
      case CIL.Div => Value.BinOp./
      case CIL.Eq  => Value.BinOp.==
      case CIL.Ne  => Value.BinOp.!=
      case CIL.Le  => Value.BinOp.<=
      case CIL.Ge  => Value.BinOp.>=
      case CIL.Lt  => Value.BinOp.<
      case CIL.Gt  => Value.BinOp.>

    }
  }
  def transformStmt(stmt: CIL.CILStmt) : List[Statement.CoreStmt] = {
    stmt match {
      case CIL.If(cond,thenBlock,elseBlock) =>
        val condV = transformValue(cond).asInstanceOf[Value.CondBinExp].negate
        val thenB = thenBlock.stmts.flatMap{transformStmt}
        val elseB = elseBlock.stmts.flatMap{transformStmt}
        val targetNop = Statement.Nop()
        val lastNop = Statement.Nop()
        val ifstmt = Statement.If(condV,new Statement.Target().setStmt(targetNop))
        val gotostmt = Statement.Goto(new Statement.Target().setStmt(lastNop))
        List(ifstmt)++thenB++List(gotostmt,targetNop)++elseB++List(lastNop)
      case CIL.While(cond,loop) =>
        val condV = transformValue(cond).asInstanceOf[Value.CondBinExp].negate
        val loopB = loop.stmts.flatMap{transformStmt}
        val lastNop = Statement.Nop()
        val firstNop = Statement.Nop()
        val ifstmt = Statement.If(condV,new Statement.Target().setStmt(lastNop))
        val goback = Statement.Goto(new Statement.Target().setStmt(firstNop))
        List(firstNop,ifstmt)++loopB++List(goback,lastNop)
      case CIL.Assign(lv,rv) =>
        val loc = transformValue(lv).asInstanceOf[Value.Loc]
        val value = transformValue(rv)
        List(Statement.Assign(loc,value))
      case CIL.Invoke(ret,callee,params) =>
        val retLocalOpt = ret.map{transformValue}.map{_.asInstanceOf[Value.Local]}
        //val invokeType = Type.StaticInvoke()
        ???
      case CIL.Return(v) =>
        val retLoc = v.map{transformValue}.map{_.asInstanceOf[Value.Loc]}
        List(Statement.Return(retLoc))
    }
  }
  def transformMethod(clsName: ClassName)(mtd: CIL.Method) : (MethodSig,Method) = {
    val methodName = mtd.name.id
    val methodParams = mtd.params.map{x=>transformType(x._2)}
    val methodSig = MethodSig(clsName,methodName,methodParams)
    val body = mtd.body.stmts.flatMap{transformStmt}
    (methodSig,Method(methodSig,body))
  }
  def transform(cls: CIL.Clazz) : Clazz = {
    val className = ClassName(cls.name.id)
    val methods = cls.methods.map{transformMethod(className)}.toMap
    Clazz(className, methods, Set.empty, ClassName("java.lang.Object"))
  }
}
