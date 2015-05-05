package com.simplytyped.yoyak.parser.cil

import com.simplytyped.yoyak.il.CommonIL._
import com.simplytyped.yoyak.parser.cil.{CommonILParser => CIL}

class CommonILTransform {
  private var varIdx = 0
  private val thisLocal = Value.Local("$this")
  def getTmpVar() : Value.Local = { varIdx += 1; Value.Local(s"_tmp$varIdx") }
  def transformType(ty: CIL.Type) : Type.ValueType = {
    ty match {
      case CIL.IntegerType => Type.IntegerType
      case CIL.StringType => Type.CommonTypes.String
    }
  }
  def transformValue(vl: CIL.Value) : (Value.Loc, List[Statement.CoreStmt]) = {
    vl match {
      case CIL.Ident(id) => (Value.Local(id), List())
      case CIL.CInteger(v) =>
        val tmp = getTmpVar()
        (tmp, List(Statement.Assign(tmp,Value.IntegerConstant(v))))
      case CIL.CString(v) =>
        val tmp = getTmpVar()
        (tmp, List(Statement.Assign(tmp,Value.StringConstant(v))))
      case exp : CIL.BinExp =>
        val (rv,stmt) = transformExp(exp)
        val lv = getTmpVar()
        (lv, stmt++List(Statement.Assign(lv,rv)))
    }
  }
  def transformExp(exp: CIL.BinExp) : (Value.BinExp, List[Statement.CoreStmt]) = {
    val (lv,lvStmt) = transformValue(exp.lv)
    val (rv,rvStmt) = transformValue(exp.rv)
    val op = transformOp(exp.op)
    op match {
      case o : Value.BinOp.CompOp => (Value.CompBinExp(lv,o,rv),lvStmt++rvStmt)
      case o : Value.BinOp.CondOp => (Value.CondBinExp(lv,o,rv),lvStmt++rvStmt)
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
  def transformStmt(methodSigMap: Map[String,MethodSig])(stmt: CIL.CILStmt) : List[Statement.CoreStmt] = {
    stmt match {
      case CIL.If(cond,thenBlock,elseBlock) =>
        val (condLoc,condStmt) = transformValue(cond)
        val condExp = Value.CondBinExp(condLoc,Value.BinOp.!=,Value.BooleanConstant(true))
        val thenB = thenBlock.stmts.flatMap{transformStmt(methodSigMap)}
        val elseB = elseBlock.stmts.flatMap{transformStmt(methodSigMap)}
        val targetNop = Statement.Nop()
        val lastNop = Statement.Nop()
        val ifstmt = Statement.If(condExp,new Statement.Target().setStmt(targetNop))
        val gotostmt = Statement.Goto(new Statement.Target().setStmt(lastNop))
        condStmt++List(ifstmt)++thenB++List(gotostmt,targetNop)++elseB++List(lastNop)
      case CIL.While(cond,loop) =>
        val (condLoc,condStmt) = transformValue(cond)
        val condExp = Value.CondBinExp(condLoc,Value.BinOp.!=,Value.BooleanConstant(true))
        val loopB = loop.stmts.flatMap{transformStmt(methodSigMap)}
        val lastNop = Statement.Nop()
        val firstNop = Statement.Nop()
        val ifstmt = Statement.If(condExp,new Statement.Target().setStmt(lastNop))
        val goback = Statement.Goto(new Statement.Target().setStmt(firstNop))
        List(firstNop)++condStmt++List(ifstmt)++loopB++List(goback,lastNop)
      case CIL.Assign(lv,rv) =>
        val (loc,_) = transformValue(lv)
        val (value,valueStmt) = transformValue(rv)
        valueStmt++List(Statement.Assign(loc,value))
      case CIL.Invoke(ret,callee,args) =>
        val retLocalOpt = ret.map{transformValue}.map{_._1.asInstanceOf[Value.Local]}
        val targetSig = methodSigMap(callee.id)
        val (argV,argStmt) = args.foldLeft(List.empty[Value.Loc],List.empty[Statement.CoreStmt]) {
          case ((vl,sl),a) =>
            val (loc,stmt) = transformValue(a)
            (vl:+loc,sl++stmt)
        }
        val invokeType = Type.DynamicInvoke(targetSig,argV,thisLocal)
        argStmt++List(Statement.Invoke(retLocalOpt,invokeType))
      case CIL.Return(v) =>
        val retLocStmt = v.map{transformValue}
        if(retLocStmt.nonEmpty) retLocStmt.get._2++List(Statement.Return(Some(retLocStmt.get._1)))
        else List(Statement.Return(None))

    }
  }
  def transformMethod(clsName: ClassName, methodSigMap: Map[String,MethodSig])(mtd: CIL.Method) : (MethodSig,Method) = {
    val methodName = mtd.name.id
    val (paramAssigns,methodParamTypes) = mtd.params.zipWithIndex.foldLeft(List.empty[Statement.CoreStmt],List.empty[Type.ValueType]) {
      case ((al,tl),((i,t),idx)) => (al:+Statement.Assign(Value.Local(i.id),Value.Param(idx)),tl:+transformType(t))
    }
    val methodSig = MethodSig(clsName,methodName,methodParamTypes)
    val body = mtd.body.stmts.flatMap{transformStmt(methodSigMap)}
    val assigns = Statement.Assign(thisLocal,Value.This)::paramAssigns
    (methodSig,Method(methodSig,assigns++body))
  }
  def extractSigs(clsName: ClassName)(mtd: CIL.Method) : (String,MethodSig) = {
    val methodName = mtd.name.id
    val methodParams = mtd.params.map{x=>transformType(x._2)}
    (methodName, MethodSig(clsName,methodName,methodParams))
  }
  def transform(cls: CIL.Clazz) : Clazz = {
    val className = ClassName(cls.name.id)
    val methodSigMap = cls.methods.map{extractSigs(className)}.toMap
    val methods = cls.methods.map{transformMethod(className,methodSigMap)}.toMap
    Clazz(className, methods, Set.empty, ClassName("java.lang.Object"))
  }
}
