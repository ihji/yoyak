package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.CommonIL.Statement.Stmt.StmtCopier
import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Type._
import com.simplytyped.yoyak.il.CommonIL.Value
import com.simplytyped.yoyak.il.CommonIL.Value._

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
  private def substituteI(f: Value.Local => Value.Local)(v: Value.Instant) : Value.Instant = {
    v match {
      case v : Loc => substituteL(f)(v)
      case _ => v
    }
  }
  private def substituteV(f: Value.Local => Value.Local)(v: Value.t) : Value.t = {
    v match {
      case v : CastExp => v.copy(v = substituteL(f)(v.v))
      case v : InstanceOfExp => v.copy(v = substituteL(f)(v.v))
      case v : LengthExp => v.copy(v = substituteL(f)(v.v))
      case v : NewArrayExp => v.copy(size = substituteI(f)(v.size))
      case v : Instant => substituteI(f)(v)
      case v : CondBinExp => v.copy(lv = substituteV(f)(v.lv), rv = substituteV(f)(v.rv))
      case v : CompBinExp => v.copy(lv = substituteV(f)(v.lv), rv = substituteV(f)(v.rv))
      case _ => v
    }
  }
  private def substituteL(f: Value.Local => Value.Local)(v: Value.Loc) : Value.Loc = {
    v match {
      case v : Local => f(v)
      case v : ArrayRef => v.copy(base = substituteL(f)(v.base), index = substituteI(f)(v.index))
      case v : InstanceFieldRef => v.copy(base = substituteL(f)(v.base))
      case v : StaticFieldRef => v
    }
  }
  def substituteLocalDef(f: Value.Local => Value.Local)(stmt: CoreStmt) : CoreStmt = {
    stmt match {
      case stmt@Identity(lv, rv) =>
        StmtCopier.Identity(stmt,f(lv),rv)
      case stmt@Assign(lv, rv) =>
        val newLv = if(lv.isInstanceOf[Local]) f(lv.asInstanceOf[Local]) else lv
        StmtCopier.Assign(stmt,newLv,rv)
      case stmt@Invoke(ret, callee) =>
        val newRet = ret.map{f}
        StmtCopier.Invoke(stmt,newRet,callee)
      case _ => stmt
    }
  }
  def substituteLocalUse(f: Value.Local => Value.Local)(stmt: CoreStmt) : CoreStmt = {
    stmt match {
      case stmt@If(CondBinExp(v1,op,v2), target) =>
        val newCond = CondBinExp(substituteV(f)(v1),op,substituteV(f)(v2))
        StmtCopier.If(stmt,newCond,target)
      case stmt@Goto(target) => stmt
      case stmt@Identity(lv, rv) => stmt
      case stmt@Assign(lv, rv) =>
        val newLv = if(lv.isInstanceOf[Local]) lv else substituteL(f)(lv)
        StmtCopier.Assign(stmt,newLv,substituteV(f)(rv))
      case stmt@Invoke(ret, callee) =>
        val newCallee = callee match {
          case inv: DynamicInvoke => inv.copy(args = inv.args.map{substituteV(f)}, base = substituteL(f)(inv.base))
          case inv: StaticInvoke => inv.copy(args = inv.args.map{substituteV(f)})
        }
        StmtCopier.Invoke(stmt,ret,newCallee)
      case stmt@Assume(CondBinExp(v1,op,v2)) =>
        val newCond = CondBinExp(substituteV(f)(v1),op,substituteV(f)(v2))
        StmtCopier.Assume(stmt,newCond)
      case stmt@Return(v) =>
        StmtCopier.Return(stmt,v.map{substituteV(f)})
      case stmt@Nop() => stmt
      case stmt@EnterMonitor(v) =>
        StmtCopier.EnterMonitor(stmt,substituteL(f)(v))
      case stmt@ExitMonitor(v) =>
        StmtCopier.ExitMonitor(stmt,substituteL(f)(v))
      case stmt@Throw(v) =>
        StmtCopier.Throw(stmt,substituteL(f)(v))
    }
  }
}
