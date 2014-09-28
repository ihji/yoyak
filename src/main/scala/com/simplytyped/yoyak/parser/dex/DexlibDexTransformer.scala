package com.simplytyped.yoyak.parser.dex

import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Type
import com.simplytyped.yoyak.il.CommonIL.Type.{StaticInvoke, DynamicInvoke}
import com.simplytyped.yoyak.il.CommonIL.Value._
import com.simplytyped.yoyak.il.CommonILHelper
import com.simplytyped.yoyak.parser.dex.DexlibDexTransformer._
import org.jf.dexlib2.{AccessFlags, Opcode}
import org.jf.dexlib2.dexbacked.instruction._
import org.jf.dexlib2.iface.instruction.Instruction
import org.jf.dexlib2.iface.reference.{MethodReference, FieldReference, TypeReference, StringReference}

import scala.collection.JavaConverters._
import com.simplytyped.yoyak.il.CommonIL._
import org.jf.dexlib2.dexbacked.{DexBackedMethod, DexBackedClassDef, DexBackedDexFile}

class DexlibDexTransformer {
  def typeTransform(ty: String) : Type.ValueType = {
    ty match {
      case "V" => Type.VoidType
      case "Z" => Type.BooleanType
      case "C" => Type.CharType
      case "B" => Type.ByteType
      case "S" => Type.ShortType
      case "I" => Type.IntegerType
      case "J" => Type.LongType
      case "F" => Type.FloatType
      case "D" => Type.DoubleType
      case _ if ty.startsWith("[") =>
        val dim = ty.takeWhile(_ == '[').length
        val baseTy = typeTransform(ty.substring(dim))
        Type.ArrayType(baseTy, dim)
      case _ if ty.startsWith("L") && ty.endsWith(";") =>
        Type.RefType(ClassName(ty.substring(1,ty.length-1).replace("/",".")))
    }
  }
  def instructionTransform(instrIdx: (Instruction,Int))(implicit context: Context) : Stmt = {
    val (instr,idx) = instrIdx
    instr.getOpcode match {
      case Opcode.NOP => Nop()
      case Opcode.MOVE | Opcode.MOVE_WIDE | Opcode.MOVE_OBJECT =>
        val move = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(move.getRegisterA)
        val src = getRegVar(move.getRegisterB)
        Assign(dest,src)
      case Opcode.MOVE_FROM16 | Opcode.MOVE_WIDE_FROM16 | Opcode.MOVE_OBJECT_FROM16 =>
        val move = instr.asInstanceOf[DexBackedInstruction22x]
        val dest = getRegVar(move.getRegisterA)
        val src = getRegVar(move.getRegisterB)
        Assign(dest,src)
      case Opcode.MOVE_16 | Opcode.MOVE_WIDE_16 | Opcode.MOVE_OBJECT_16 =>
        val move = instr.asInstanceOf[DexBackedInstruction32x]
        val dest = getRegVar(move.getRegisterA)
        val src = getRegVar(move.getRegisterB)
        Assign(dest,src)
      case Opcode.MOVE_RESULT | Opcode.MOVE_RESULT_WIDE | Opcode.MOVE_RESULT_OBJECT =>
        val move = instr.asInstanceOf[DexBackedInstruction11x]
        val dest = getRegVar(move.getRegisterA)
        Assign(dest,methodReturnVar)
      case Opcode.MOVE_EXCEPTION =>
        val move = instr.asInstanceOf[DexBackedInstruction11x]
        val dest = getRegVar(move.getRegisterA)
        Assign(dest,CaughtExceptionRef)
      case Opcode.RETURN_VOID =>
        Return(None)
      case Opcode.RETURN | Opcode.RETURN_WIDE | Opcode.RETURN_OBJECT =>
        val ret = instr.asInstanceOf[DexBackedInstruction11x]
        val retVal = getRegVar(ret.getRegisterA)
        Return(Some(retVal))
      case Opcode.CONST_4 =>
        val const = instr.asInstanceOf[DexBackedInstruction11n]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getNarrowLiteral
        Assign(dest,IntegerConstant(constVal))
      case Opcode.CONST_16 | Opcode.CONST_WIDE_16 =>
        val const = instr.asInstanceOf[DexBackedInstruction21s]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getNarrowLiteral
        Assign(dest,IntegerConstant(constVal))
      case Opcode.CONST | Opcode.CONST_WIDE_32 =>
        val const = instr.asInstanceOf[DexBackedInstruction31i]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getNarrowLiteral
        Assign(dest,IntegerConstant(constVal))
      case Opcode.CONST_HIGH16 =>
        val const = instr.asInstanceOf[DexBackedInstruction21ih]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getNarrowLiteral
        Assign(dest,IntegerConstant(constVal))
      case Opcode.CONST_WIDE =>
        val const = instr.asInstanceOf[DexBackedInstruction51l]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getWideLiteral
        Assign(dest,LongConstant(constVal))
      case Opcode.CONST_WIDE_HIGH16 =>
        val const = instr.asInstanceOf[DexBackedInstruction21lh]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getWideLiteral
        Assign(dest,LongConstant(constVal))
      case Opcode.CONST_STRING =>
        val const = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(const.getRegisterA).setType(Type.CommonTypes.String)
        val constVal = const.getReference.asInstanceOf[StringReference].getString
        Assign(dest,StringConstant(constVal))
      case Opcode.CONST_STRING_JUMBO =>
        val const = instr.asInstanceOf[DexBackedInstruction31c]
        val dest = getRegVar(const.getRegisterA).setType(Type.CommonTypes.String)
        val constVal = const.getReference.asInstanceOf[StringReference].getString
        Assign(dest,StringConstant(constVal))
      case Opcode.CONST_CLASS =>
        val const = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(const.getRegisterA)
        val refType = typeTransform(const.getReference.asInstanceOf[TypeReference].getType).asInstanceOf[Type.RefType]
        Assign(dest,ClassConstant(refType.className))
      case Opcode.MONITOR_ENTER =>
        val mon = instr.asInstanceOf[DexBackedInstruction11x]
        val loc = getRegVar(mon.getRegisterA)
        EnterMonitor(loc)
      case Opcode.MONITOR_EXIT =>
        val mon = instr.asInstanceOf[DexBackedInstruction11x]
        val loc = getRegVar(mon.getRegisterA)
        ExitMonitor(loc)
      case Opcode.CHECK_CAST =>
        val check = instr.asInstanceOf[DexBackedInstruction21c]
        val ty = typeTransform(check.getReference.asInstanceOf[TypeReference].getType)
        val loc = getRegVar(check.getRegisterA).setType(ty)
        Assign(loc,CastExp(loc,ty).setType(ty))
      case Opcode.INSTANCE_OF =>
        val instanceOf = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(instanceOf.getRegisterA).setType(Type.BooleanType)
        val loc = getRegVar(instanceOf.getRegisterB)
        val ty = typeTransform(instanceOf.getReference.asInstanceOf[TypeReference].getType).asInstanceOf[Type.RefType]
        Assign(dest,InstanceOfExp(loc,ty).setType(Type.BooleanType))
      case Opcode.ARRAY_LENGTH =>
        val length = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(length.getRegisterA).setType(Type.IntegerType)
        val loc = getRegVar(length.getRegisterB)
        Assign(dest,LengthExp(loc).setType(Type.IntegerType))
      case Opcode.NEW_INSTANCE =>
        val newInstance = instr.asInstanceOf[DexBackedInstruction21c]
        val ty = typeTransform(newInstance.getReference.asInstanceOf[TypeReference].getType).asInstanceOf[Type.RefType]
        val dest = getRegVar(newInstance.getRegisterA).setType(ty)
        Assign(dest,NewExp(ty).setType(ty))
      case Opcode.NEW_ARRAY =>
        val newArray = instr.asInstanceOf[DexBackedInstruction22c]
        val ty = typeTransform(newArray.getReference.asInstanceOf[TypeReference].getType)
        val dest = getRegVar(newArray.getRegisterA).setType(ty)
        val size = getRegVar(newArray.getRegisterB).setType(Type.IntegerType)
        Assign(dest,NewArrayExp(ty,size).setType(ty))
      case Opcode.FILLED_NEW_ARRAY =>
        val newArray = instr.asInstanceOf[DexBackedInstruction35c]
        val ty = typeTransform(newArray.getReference.asInstanceOf[TypeReference].getType)
        val size = newArray.getRegisterCount
        val args = List(getRegVar(newArray.getRegisterC),getRegVar(newArray.getRegisterD),getRegVar(newArray.getRegisterE),getRegVar(newArray.getRegisterF),getRegVar(newArray.getRegisterG)).take(size)
        val stmts = Assign(methodReturnVar,NewArrayExp(ty,IntegerConstant(size)))::args.zipWithIndex.map{case (v,idx) => Assign(ArrayRef(methodReturnVar,IntegerConstant(idx)),v)}
        Block(new StatementContainer().setStmts(stmts))
      case Opcode.FILLED_NEW_ARRAY_RANGE =>
        val newArray = instr.asInstanceOf[DexBackedInstruction3rc]
        val ty = typeTransform(newArray.getReference.asInstanceOf[TypeReference].getType)
        val startRegister = newArray.getStartRegister
        val registerCount = newArray.getRegisterCount
        val args = (startRegister until startRegister+registerCount).map{getRegVar}.toList
        val stmts = Assign(methodReturnVar,NewArrayExp(ty,IntegerConstant(registerCount)))::args.zipWithIndex.map{case (v,idx) => Assign(ArrayRef(methodReturnVar,IntegerConstant(idx)),v)}
        Block(new StatementContainer().setStmts(stmts))
      case Opcode.FILL_ARRAY_DATA =>
        val fillArray = instr.asInstanceOf[DexBackedInstruction31t]
        val array = getRegVar(fillArray.getRegisterA)
        val targetIndex = offsetToIndex(idx,fillArray.getCodeOffset)
        val arrayDataList = context.instrs(targetIndex).asInstanceOf[DexBackedArrayPayload].getArrayElements.asScala.toList
        val stmts = arrayDataList.zipWithIndex.map{case (d,idx) => Assign(ArrayRef(array,IntegerConstant(idx)),IntegerConstant(d.intValue))}
        Block(new StatementContainer().setStmts(stmts))
      case Opcode.THROW =>
        val thr = instr.asInstanceOf[DexBackedInstruction11x]
        val loc = getRegVar(thr.getRegisterA)
        Throw(loc)
      case Opcode.GOTO =>
        val goto = instr.asInstanceOf[DexBackedInstruction10t]
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,goto.getCodeOffset))))
        Goto(target)
      case Opcode.GOTO_16 =>
        val goto = instr.asInstanceOf[DexBackedInstruction20t]
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,goto.getCodeOffset))))
        Goto(target)
      case Opcode.GOTO_32 =>
        val goto = instr.asInstanceOf[DexBackedInstruction30t]
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,goto.getCodeOffset))))
        Goto(target)
      case Opcode.PACKED_SWITCH =>
        val switch = instr.asInstanceOf[DexBackedInstruction31t]
        val test = getRegVar(switch.getRegisterA)
        val targetIndex = offsetToIndex(idx,switch.getCodeOffset)
        val payload = context.instrs(targetIndex).asInstanceOf[DexBackedPackedSwitchPayload]
        val table = payload.getSwitchElements.asScala.toList
        val revLists = table.foldLeft(List.empty[Int], List.empty[Int]){ case ((ks,vs),x) => (x.getKey::ks,x.getOffset::vs) }
        val keys = revLists._1.reverse.map{IntegerConstant}
        val targets = revLists._2.reverse.map{x => new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,x))))}
        Switch(test,keys,targets)
      case Opcode.SPARSE_SWITCH =>
        val switch = instr.asInstanceOf[DexBackedInstruction31t]
        val test = getRegVar(switch.getRegisterA)
        val targetIndex = offsetToIndex(idx,switch.getCodeOffset)
        val payload = context.instrs(targetIndex).asInstanceOf[DexBackedSparseSwitchPayload]
        val table = payload.getSwitchElements.asScala.toList
        val revLists = table.foldLeft(List.empty[Int], List.empty[Int]){ case ((ks,vs),x) => (x.getKey::ks,x.getOffset::vs) }
        val keys = revLists._1.reverse.map{IntegerConstant}
        val targets = revLists._2.reverse.map{x => new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,x))))}
        Switch(test,keys,targets)
      case Opcode.CMPL_FLOAT =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(cmp.getRegisterB).setType(Type.FloatType)
        val second = getRegVar(cmp.getRegisterC).setType(Type.FloatType)
        Assign(dest,CompBinExp(first,BinOp.cmpl,second).setType(Type.IntegerType))
      case Opcode.CMPG_FLOAT =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(cmp.getRegisterB).setType(Type.FloatType)
        val second = getRegVar(cmp.getRegisterC).setType(Type.FloatType)
        Assign(dest,CompBinExp(first,BinOp.cmpg,second).setType(Type.IntegerType))
      case Opcode.CMPL_DOUBLE =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(cmp.getRegisterB).setType(Type.DoubleType)
        val second = getRegVar(cmp.getRegisterC).setType(Type.DoubleType)
        Assign(dest,CompBinExp(first,BinOp.cmpl,second).setType(Type.IntegerType))
      case Opcode.CMPG_DOUBLE =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(cmp.getRegisterB).setType(Type.DoubleType)
        val second = getRegVar(cmp.getRegisterC).setType(Type.DoubleType)
        Assign(dest,CompBinExp(first,BinOp.cmpg,second).setType(Type.IntegerType))
      case Opcode.CMP_LONG =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(cmp.getRegisterB).setType(Type.LongType)
        val second = getRegVar(cmp.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.cmp,second).setType(Type.IntegerType))
      case Opcode.IF_EQ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(first,BinOp.==,second),target)
      case Opcode.IF_NE =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(first,BinOp.!=,second),target)
      case Opcode.IF_LT =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(first,BinOp.<,second),target)
      case Opcode.IF_GE =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(first,BinOp.>=,second),target)
      case Opcode.IF_GT =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(first,BinOp.>,second),target)
      case Opcode.IF_LE =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(first,BinOp.<=,second),target)
      case Opcode.IF_EQZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(test,BinOp.==,IntegerConstant(0)),target)
      case Opcode.IF_NEZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(test,BinOp.!=,IntegerConstant(0)),target)
      case Opcode.IF_LTZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(test,BinOp.<,IntegerConstant(0)),target)
      case Opcode.IF_GEZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(test,BinOp.>=,IntegerConstant(0)),target)
      case Opcode.IF_GTZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(test,BinOp.>,IntegerConstant(0)),target)
      case Opcode.IF_LEZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val target = new Target().setStmt(Placeholder(context.instrs(offsetToIndex(idx,ifcond.getCodeOffset))))
        If(CondBinExp(test,BinOp.<=,IntegerConstant(0)),target)
      case Opcode.AGET =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC).setType(Type.IntegerType)
        Assign(dest,ArrayRef(base,index))
      case Opcode.AGET_WIDE =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC).setType(Type.IntegerType)
        Assign(dest,ArrayRef(base,index))
      case Opcode.AGET_OBJECT =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC).setType(Type.IntegerType)
        Assign(dest,ArrayRef(base,index))
      case Opcode.AGET_BOOLEAN =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA).setType(Type.BooleanType)
        val base = getRegVar(get.getRegisterB).setType(Type.ArrayType(Type.BooleanType,1))
        val index = getRegVar(get.getRegisterC).setType(Type.IntegerType)
        Assign(dest,ArrayRef(base,index).setType(Type.BooleanType))
      case Opcode.AGET_BYTE =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA).setType(Type.ByteType)
        val base = getRegVar(get.getRegisterB).setType(Type.ArrayType(Type.ByteType,1))
        val index = getRegVar(get.getRegisterC).setType(Type.IntegerType)
        Assign(dest,ArrayRef(base,index).setType(Type.ByteType))
      case Opcode.AGET_CHAR =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA).setType(Type.CharType)
        val base = getRegVar(get.getRegisterB).setType(Type.ArrayType(Type.CharType,1))
        val index = getRegVar(get.getRegisterC).setType(Type.IntegerType)
        Assign(dest,ArrayRef(base,index).setType(Type.CharType))
      case Opcode.AGET_SHORT =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA).setType(Type.ShortType)
        val base = getRegVar(get.getRegisterB).setType(Type.ArrayType(Type.ShortType,1))
        val index = getRegVar(get.getRegisterC).setType(Type.IntegerType)
        Assign(dest,ArrayRef(base,index).setType(Type.ShortType))
      case Opcode.APUT =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC).setType(Type.IntegerType)
        Assign(ArrayRef(base,index),src)
      case Opcode.APUT_WIDE =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC).setType(Type.IntegerType)
        Assign(ArrayRef(base,index),src)
      case Opcode.APUT_OBJECT =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC).setType(Type.IntegerType)
        Assign(ArrayRef(base,index),src)
      case Opcode.APUT_BOOLEAN =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA).setType(Type.BooleanType)
        val base = getRegVar(put.getRegisterB).setType(Type.ArrayType(Type.BooleanType,1))
        val index = getRegVar(put.getRegisterC).setType(Type.IntegerType)
        Assign(ArrayRef(base,index).setType(Type.BooleanType),src)
      case Opcode.APUT_BYTE =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA).setType(Type.ByteType)
        val base = getRegVar(put.getRegisterB).setType(Type.ArrayType(Type.ByteType,1))
        val index = getRegVar(put.getRegisterC).setType(Type.IntegerType)
        Assign(ArrayRef(base,index).setType(Type.ByteType),src)
      case Opcode.APUT_CHAR =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA).setType(Type.CharType)
        val base = getRegVar(put.getRegisterB).setType(Type.ArrayType(Type.CharType,1))
        val index = getRegVar(put.getRegisterC).setType(Type.IntegerType)
        Assign(ArrayRef(base,index).setType(Type.CharType),src)
      case Opcode.APUT_SHORT =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA).setType(Type.ShortType)
        val base = getRegVar(put.getRegisterB).setType(Type.ArrayType(Type.ShortType,1))
        val index = getRegVar(put.getRegisterC).setType(Type.IntegerType)
        Assign(ArrayRef(base,index).setType(Type.ShortType),src)
      case Opcode.IGET =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val field = get.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val fieldTy = typeTransform(field.getType)
        val className = ClassName(field.getDefiningClass)
        val dest = getRegVar(get.getRegisterA).setType(fieldTy)
        val base = getRegVar(get.getRegisterB).setType(Type.RefType(className))
        Assign(dest,InstanceFieldRef(base,fieldName).setType(fieldTy))
      case Opcode.IGET_WIDE =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val field = get.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val fieldTy = typeTransform(field.getType)
        val className = ClassName(field.getDefiningClass)
        val dest = getRegVar(get.getRegisterA).setType(fieldTy)
        val base = getRegVar(get.getRegisterB).setType(Type.RefType(className))
        Assign(dest,InstanceFieldRef(base,fieldName).setType(fieldTy))
      case Opcode.IGET_OBJECT =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val field = get.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val fieldTy = typeTransform(field.getType)
        val className = ClassName(field.getDefiningClass)
        val dest = getRegVar(get.getRegisterA).setType(fieldTy)
        val base = getRegVar(get.getRegisterB).setType(Type.RefType(className))
        Assign(dest,InstanceFieldRef(base,fieldName).setType(fieldTy))
      case Opcode.IGET_BOOLEAN =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA).setType(Type.BooleanType)
        val field = get.getReference.asInstanceOf[FieldReference]
        val fieldName = get.getReference.asInstanceOf[FieldReference].getName
        val className = ClassName(field.getDefiningClass)
        val base = getRegVar(get.getRegisterB).setType(Type.RefType(className))
        Assign(dest,InstanceFieldRef(base,fieldName).setType(Type.BooleanType))
      case Opcode.IGET_BYTE =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA).setType(Type.ByteType)
        val field = get.getReference.asInstanceOf[FieldReference]
        val fieldName = get.getReference.asInstanceOf[FieldReference].getName
        val className = ClassName(field.getDefiningClass)
        val base = getRegVar(get.getRegisterB).setType(Type.RefType(className))
        Assign(dest,InstanceFieldRef(base,fieldName).setType(Type.ByteType))
      case Opcode.IGET_CHAR =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA).setType(Type.CharType)
        val field = get.getReference.asInstanceOf[FieldReference]
        val fieldName = get.getReference.asInstanceOf[FieldReference].getName
        val className = ClassName(field.getDefiningClass)
        val base = getRegVar(get.getRegisterB).setType(Type.RefType(className))
        Assign(dest,InstanceFieldRef(base,fieldName).setType(Type.CharType))
      case Opcode.IGET_SHORT =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA).setType(Type.ShortType)
        val field = get.getReference.asInstanceOf[FieldReference]
        val fieldName = get.getReference.asInstanceOf[FieldReference].getName
        val className = ClassName(field.getDefiningClass)
        val base = getRegVar(get.getRegisterB).setType(Type.RefType(className))
        Assign(dest,InstanceFieldRef(base,fieldName).setType(Type.ShortType))
      case Opcode.IPUT =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val field = put.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val fieldTy = typeTransform(field.getType)
        val className = ClassName(field.getDefiningClass)
        val src = getRegVar(put.getRegisterA).setType(fieldTy)
        val base = getRegVar(put.getRegisterB).setType(Type.RefType(className))
        Assign(InstanceFieldRef(base,fieldName).setType(fieldTy),src)
      case Opcode.IPUT_WIDE =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val field = put.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val fieldTy = typeTransform(field.getType)
        val className = ClassName(field.getDefiningClass)
        val src = getRegVar(put.getRegisterA).setType(fieldTy)
        val base = getRegVar(put.getRegisterB).setType(Type.RefType(className))
        Assign(InstanceFieldRef(base,fieldName).setType(fieldTy),src)
      case Opcode.IPUT_OBJECT =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val field = put.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val fieldTy = typeTransform(field.getType)
        val className = ClassName(field.getDefiningClass)
        val src = getRegVar(put.getRegisterA).setType(fieldTy)
        val base = getRegVar(put.getRegisterB).setType(Type.RefType(className))
        Assign(InstanceFieldRef(base,fieldName).setType(fieldTy),src)
      case Opcode.IPUT_BOOLEAN =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA).setType(Type.BooleanType)
        val field = put.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val className = ClassName(field.getDefiningClass)
        val base = getRegVar(put.getRegisterB).setType(Type.RefType(className))
        Assign(InstanceFieldRef(base,fieldName).setType(Type.BooleanType),src)
      case Opcode.IPUT_BYTE =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA).setType(Type.ByteType)
        val field = put.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val className = ClassName(field.getDefiningClass)
        val base = getRegVar(put.getRegisterB).setType(Type.RefType(className))
        Assign(InstanceFieldRef(base,fieldName).setType(Type.ByteType),src)
      case Opcode.IPUT_CHAR =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA).setType(Type.CharType)
        val field = put.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val className = ClassName(field.getDefiningClass)
        val base = getRegVar(put.getRegisterB).setType(Type.RefType(className))
        Assign(InstanceFieldRef(base,fieldName).setType(Type.CharType),src)
      case Opcode.IPUT_SHORT =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA).setType(Type.ShortType)
        val field = put.getReference.asInstanceOf[FieldReference]
        val fieldName = field.getName
        val className = ClassName(field.getDefiningClass)
        val base = getRegVar(put.getRegisterB).setType(Type.RefType(className))
        Assign(InstanceFieldRef(base,fieldName).setType(Type.ShortType),src)
      case Opcode.SGET =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        val fieldTy = typeTransform(fieldRef.getType)
        val dest = getRegVar(get.getRegisterA).setType(fieldTy)
        Assign(dest,StaticFieldRef(className,fieldName).setType(fieldTy))
      case Opcode.SGET_WIDE =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        val fieldTy = typeTransform(fieldRef.getType)
        val dest = getRegVar(get.getRegisterA).setType(fieldTy)
        Assign(dest,StaticFieldRef(className,fieldName).setType(fieldTy))
      case Opcode.SGET_OBJECT =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        val fieldTy = typeTransform(fieldRef.getType)
        val dest = getRegVar(get.getRegisterA).setType(fieldTy)
        Assign(dest,StaticFieldRef(className,fieldName).setType(fieldTy))
      case Opcode.SGET_BOOLEAN =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA).setType(Type.BooleanType)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName).setType(Type.BooleanType))
      case Opcode.SGET_BYTE =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA).setType(Type.ByteType)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName).setType(Type.ByteType))
      case Opcode.SGET_CHAR =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA).setType(Type.CharType)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName).setType(Type.CharType))
      case Opcode.SGET_SHORT =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA).setType(Type.ShortType)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName).setType(Type.ShortType))
      case Opcode.SPUT =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        val fieldTy = typeTransform(fieldRef.getType)
        val src = getRegVar(put.getRegisterA).setType(fieldTy)
        Assign(StaticFieldRef(className,fieldName).setType(fieldTy),src)
      case Opcode.SPUT_WIDE =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        val fieldTy = typeTransform(fieldRef.getType)
        val src = getRegVar(put.getRegisterA).setType(fieldTy)
        Assign(StaticFieldRef(className,fieldName).setType(fieldTy),src)
      case Opcode.SPUT_OBJECT =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        val fieldTy = typeTransform(fieldRef.getType)
        val src = getRegVar(put.getRegisterA).setType(fieldTy)
        Assign(StaticFieldRef(className,fieldName).setType(fieldTy),src)
      case Opcode.SPUT_BOOLEAN =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA).setType(Type.BooleanType)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName).setType(Type.BooleanType),src)
      case Opcode.SPUT_BYTE =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA).setType(Type.ByteType)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName).setType(Type.ByteType),src)
      case Opcode.SPUT_CHAR =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA).setType(Type.CharType)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName).setType(Type.CharType),src)
      case Opcode.SPUT_SHORT =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA).setType(Type.ShortType)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName).setType(Type.ShortType),src)
      case Opcode.INVOKE_VIRTUAL | Opcode.INVOKE_SUPER | Opcode.INVOKE_DIRECT | Opcode.INVOKE_INTERFACE =>
        val invoke = instr.asInstanceOf[DexBackedInstruction35c]
        val method = invoke.getReference.asInstanceOf[MethodReference]

        val className = ClassName(method.getDefiningClass)
        val methodName = method.getName
        val argTypes = method.getParameterTypes.asScala.map{x=>typeTransform(x.toString)}.toList
        val methodSig = MethodSig(className,methodName,argTypes)

        val argCount = invoke.getRegisterCount
        val args = List(getRegVar(invoke.getRegisterC),getRegVar(invoke.getRegisterD),getRegVar(invoke.getRegisterE),getRegVar(invoke.getRegisterF),getRegVar(invoke.getRegisterG)).take(argCount)
        val packedArgs = removeSecondOfWideRegisters(argTypes,args.tail).zip(argTypes).map{case (r,ty) => r.setType(ty)}

        val retType = typeTransform(method.getReturnType)
        val retVar = if(retType == Type.VoidType) None else Some(methodReturnVar.setType(retType))

        Invoke(retVar,DynamicInvoke(methodSig,packedArgs,args.head.setType(Type.RefType(className))))
      case Opcode.INVOKE_STATIC =>
        val invoke = instr.asInstanceOf[DexBackedInstruction35c]
        val method = invoke.getReference.asInstanceOf[MethodReference]

        val className = ClassName(method.getDefiningClass)
        val methodName = method.getName
        val argTypes = method.getParameterTypes.asScala.map{x=>typeTransform(x.toString)}.toList
        val methodSig = MethodSig(className,methodName,argTypes)

        val argCount = invoke.getRegisterCount
        val args = List(getRegVar(invoke.getRegisterC),getRegVar(invoke.getRegisterD),getRegVar(invoke.getRegisterE),getRegVar(invoke.getRegisterF),getRegVar(invoke.getRegisterG)).take(argCount)
        val packedArgs = removeSecondOfWideRegisters(argTypes,args).zip(argTypes).map{case (r,ty) => r.setType(ty)}

        val retType = typeTransform(method.getReturnType)
        val retVar = if(retType == Type.VoidType) None else Some(methodReturnVar.setType(retType))

        Invoke(retVar,StaticInvoke(methodSig,packedArgs))
      case Opcode.INVOKE_VIRTUAL_RANGE | Opcode.INVOKE_SUPER_RANGE | Opcode.INVOKE_DIRECT_RANGE | Opcode.INVOKE_INTERFACE_RANGE =>
        val invoke = instr.asInstanceOf[DexBackedInstruction3rc]
        val startRegister = invoke.getStartRegister
        val regCount = invoke.getRegisterCount
        val method = invoke.getReference.asInstanceOf[MethodReference]

        val className = ClassName(method.getDefiningClass)
        val methodName = method.getName
        val argTypes = method.getParameterTypes.asScala.map{x=>typeTransform(x.toString)}.toList
        val methodSig = MethodSig(className,methodName,argTypes)

        val args = (startRegister until startRegister+regCount).map{getRegVar}.toList
        val packedArgs = removeSecondOfWideRegisters(argTypes,args.tail).zip(argTypes).map{case (r,ty) => r.setType(ty)}

        val retType = typeTransform(method.getReturnType)
        val retVar = if(retType == Type.VoidType) None else Some(methodReturnVar.setType(retType))

        Invoke(retVar,DynamicInvoke(methodSig,packedArgs,args.head.setType(Type.RefType(className))))
      case Opcode.INVOKE_STATIC_RANGE =>
        val invoke = instr.asInstanceOf[DexBackedInstruction3rc]
        val startRegister = invoke.getStartRegister
        val regCount = invoke.getRegisterCount
        val method = invoke.getReference.asInstanceOf[MethodReference]

        val className = ClassName(method.getDefiningClass)
        val methodName = method.getName
        val argTypes = method.getParameterTypes.asScala.map{x=>typeTransform(x.toString)}.toList
        val methodSig = MethodSig(className,methodName,argTypes)

        val args = (startRegister until startRegister+regCount).map{getRegVar}.toList
        val packedArgs = removeSecondOfWideRegisters(argTypes,args).zip(argTypes).map{case (r,ty) => r.setType(ty)}

        val retType = typeTransform(method.getReturnType)
        val retVar = if(retType == Type.VoidType) None else Some(methodReturnVar.setType(retType))

        Invoke(retVar,StaticInvoke(methodSig,packedArgs))
      case Opcode.NEG_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.IntegerType)
        val src = getRegVar(unary.getRegisterB).setType(Type.IntegerType)
        Assign(dest,CompBinExp(IntegerConstant(0),BinOp.-,src).setType(Type.IntegerType))
      case Opcode.NOT_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.IntegerType)
        val src = getRegVar(unary.getRegisterB).setType(Type.IntegerType)
        Assign(dest,CompBinExp(src,BinOp.^,IntegerConstant(-1)).setType(Type.IntegerType))
      case Opcode.NEG_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.LongType)
        val src = getRegVar(unary.getRegisterB).setType(Type.LongType)
        Assign(dest,CompBinExp(LongConstant(0),BinOp.-,src).setType(Type.LongType))
      case Opcode.NOT_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.LongType)
        val src = getRegVar(unary.getRegisterB).setType(Type.LongType)
        Assign(dest,CompBinExp(src,BinOp.^,LongConstant(-1)).setType(Type.LongType))
      case Opcode.NEG_FLOAT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.FloatType)
        val src = getRegVar(unary.getRegisterB).setType(Type.FloatType)
        Assign(dest,CompBinExp(FloatConstant(0),BinOp.-,src).setType(Type.FloatType))
      case Opcode.NEG_DOUBLE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.DoubleType)
        val src = getRegVar(unary.getRegisterB).setType(Type.DoubleType)
        Assign(dest,CompBinExp(DoubleConstant(0),BinOp.-,src).setType(Type.DoubleType))
      case Opcode.INT_TO_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.LongType)
        val src = getRegVar(unary.getRegisterB).setType(Type.IntegerType)
        Assign(dest,CastExp(src,Type.LongType).setType(Type.LongType))
      case Opcode.INT_TO_FLOAT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.FloatType)
        val src = getRegVar(unary.getRegisterB).setType(Type.IntegerType)
        Assign(dest,CastExp(src,Type.FloatType).setType(Type.FloatType))
      case Opcode.INT_TO_DOUBLE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.DoubleType)
        val src = getRegVar(unary.getRegisterB).setType(Type.IntegerType)
        Assign(dest,CastExp(src,Type.DoubleType).setType(Type.DoubleType))
      case Opcode.LONG_TO_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.IntegerType)
        val src = getRegVar(unary.getRegisterB).setType(Type.LongType)
        Assign(dest,CastExp(src,Type.IntegerType).setType(Type.IntegerType))
      case Opcode.LONG_TO_FLOAT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.FloatType)
        val src = getRegVar(unary.getRegisterB).setType(Type.LongType)
        Assign(dest,CastExp(src,Type.FloatType).setType(Type.FloatType))
      case Opcode.LONG_TO_DOUBLE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.DoubleType)
        val src = getRegVar(unary.getRegisterB).setType(Type.LongType)
        Assign(dest,CastExp(src,Type.DoubleType).setType(Type.DoubleType))
      case Opcode.FLOAT_TO_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.IntegerType)
        val src = getRegVar(unary.getRegisterB).setType(Type.FloatType)
        Assign(dest,CastExp(src,Type.IntegerType).setType(Type.IntegerType))
      case Opcode.FLOAT_TO_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.LongType)
        val src = getRegVar(unary.getRegisterB).setType(Type.FloatType)
        Assign(dest,CastExp(src,Type.LongType).setType(Type.LongType))
      case Opcode.FLOAT_TO_DOUBLE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.DoubleType)
        val src = getRegVar(unary.getRegisterB).setType(Type.FloatType)
        Assign(dest,CastExp(src,Type.DoubleType).setType(Type.DoubleType))
      case Opcode.DOUBLE_TO_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.IntegerType)
        val src = getRegVar(unary.getRegisterB).setType(Type.DoubleType)
        Assign(dest,CastExp(src,Type.IntegerType).setType(Type.IntegerType))
      case Opcode.DOUBLE_TO_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.LongType)
        val src = getRegVar(unary.getRegisterB).setType(Type.DoubleType)
        Assign(dest,CastExp(src,Type.LongType).setType(Type.LongType))
      case Opcode.DOUBLE_TO_FLOAT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.FloatType)
        val src = getRegVar(unary.getRegisterB).setType(Type.DoubleType)
        Assign(dest,CastExp(src,Type.FloatType).setType(Type.FloatType))
      case Opcode.INT_TO_BYTE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.ByteType)
        val src = getRegVar(unary.getRegisterB).setType(Type.IntegerType)
        Assign(dest,CastExp(src,Type.ByteType).setType(Type.ByteType))
      case Opcode.INT_TO_CHAR =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.CharType)
        val src = getRegVar(unary.getRegisterB).setType(Type.IntegerType)
        Assign(dest,CastExp(src,Type.CharType).setType(Type.CharType))
      case Opcode.INT_TO_SHORT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA).setType(Type.ShortType)
        val src = getRegVar(unary.getRegisterB).setType(Type.IntegerType)
        Assign(dest,CastExp(src,Type.ShortType).setType(Type.ShortType))
      case Opcode.ADD_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.+,second).setType(Type.IntegerType))
      case Opcode.SUB_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.-,second).setType(Type.IntegerType))
      case Opcode.MUL_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.*,second).setType(Type.IntegerType))
      case Opcode.DIV_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp./,second).setType(Type.IntegerType))
      case Opcode.REM_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.%,second).setType(Type.IntegerType))
      case Opcode.AND_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.&,second).setType(Type.IntegerType))
      case Opcode.OR_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.|,second).setType(Type.IntegerType))
      case Opcode.XOR_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.^,second).setType(Type.IntegerType))
      case Opcode.SHL_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.<<,second).setType(Type.IntegerType))
      case Opcode.SHR_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.>>,second).setType(Type.IntegerType))
      case Opcode.USHR_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterC).setType(Type.IntegerType)
        Assign(dest,CompBinExp(first,BinOp.>>>,second).setType(Type.IntegerType))
      case Opcode.ADD_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.+,second).setType(Type.LongType))
      case Opcode.SUB_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.-,second).setType(Type.LongType))
      case Opcode.MUL_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.*,second).setType(Type.LongType))
      case Opcode.DIV_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp./,second).setType(Type.LongType))
      case Opcode.REM_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.%,second).setType(Type.LongType))
      case Opcode.AND_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.&,second).setType(Type.LongType))
      case Opcode.OR_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.|,second).setType(Type.LongType))
      case Opcode.XOR_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.^,second).setType(Type.LongType))
      case Opcode.SHL_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.<<,second).setType(Type.LongType))
      case Opcode.SHR_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.>>,second).setType(Type.LongType))
      case Opcode.USHR_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val first = getRegVar(binary.getRegisterB).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterC).setType(Type.LongType)
        Assign(dest,CompBinExp(first,BinOp.>>>,second).setType(Type.LongType))
      case Opcode.ADD_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val first = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterC).setType(Type.FloatType)
        Assign(dest,CompBinExp(first,BinOp.+,second).setType(Type.FloatType))
      case Opcode.SUB_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val first = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterC).setType(Type.FloatType)
        Assign(dest,CompBinExp(first,BinOp.-,second).setType(Type.FloatType))
      case Opcode.MUL_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val first = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterC).setType(Type.FloatType)
        Assign(dest,CompBinExp(first,BinOp.*,second).setType(Type.FloatType))
      case Opcode.DIV_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val first = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterC).setType(Type.FloatType)
        Assign(dest,CompBinExp(first,BinOp./,second).setType(Type.FloatType))
      case Opcode.REM_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val first = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterC).setType(Type.FloatType)
        Assign(dest,CompBinExp(first,BinOp.%,second).setType(Type.FloatType))
      case Opcode.ADD_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val first = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterC).setType(Type.DoubleType)
        Assign(dest,CompBinExp(first,BinOp.+,second).setType(Type.DoubleType))
      case Opcode.SUB_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val first = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterC).setType(Type.DoubleType)
        Assign(dest,CompBinExp(first,BinOp.-,second).setType(Type.DoubleType))
      case Opcode.MUL_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val first = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterC).setType(Type.DoubleType)
        Assign(dest,CompBinExp(first,BinOp.*,second).setType(Type.DoubleType))
      case Opcode.DIV_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val first = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterC).setType(Type.DoubleType)
        Assign(dest,CompBinExp(first,BinOp./,second).setType(Type.DoubleType))
      case Opcode.REM_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val first = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterC).setType(Type.DoubleType)
        Assign(dest,CompBinExp(first,BinOp.%,second).setType(Type.DoubleType))
      case Opcode.ADD_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.+,second).setType(Type.IntegerType))
      case Opcode.SUB_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.-,second).setType(Type.IntegerType))
      case Opcode.MUL_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.*,second).setType(Type.IntegerType))
      case Opcode.DIV_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp./,second).setType(Type.IntegerType))
      case Opcode.REM_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.%,second).setType(Type.IntegerType))
      case Opcode.AND_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.&,second).setType(Type.IntegerType))
      case Opcode.OR_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.|,second).setType(Type.IntegerType))
      case Opcode.XOR_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.^,second).setType(Type.IntegerType))
      case Opcode.SHL_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.<<,second).setType(Type.IntegerType))
      case Opcode.SHR_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.>>,second).setType(Type.IntegerType))
      case Opcode.USHR_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val second = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        Assign(first,CompBinExp(first,BinOp.>>>,second).setType(Type.IntegerType))
      case Opcode.ADD_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.+,second).setType(Type.LongType))
      case Opcode.SUB_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.-,second).setType(Type.LongType))
      case Opcode.MUL_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.*,second).setType(Type.LongType))
      case Opcode.DIV_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp./,second).setType(Type.LongType))
      case Opcode.REM_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.%,second).setType(Type.LongType))
      case Opcode.AND_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.&,second).setType(Type.LongType))
      case Opcode.OR_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.|,second).setType(Type.LongType))
      case Opcode.XOR_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.^,second).setType(Type.LongType))
      case Opcode.SHL_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.<<,second).setType(Type.LongType))
      case Opcode.SHR_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.>>,second).setType(Type.LongType))
      case Opcode.USHR_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.LongType)
        val second = getRegVar(binary.getRegisterB).setType(Type.LongType)
        Assign(first,CompBinExp(first,BinOp.>>>,second).setType(Type.LongType))
      case Opcode.ADD_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        Assign(first,CompBinExp(first,BinOp.+,second).setType(Type.FloatType))
      case Opcode.SUB_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        Assign(first,CompBinExp(first,BinOp.-,second).setType(Type.FloatType))
      case Opcode.MUL_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        Assign(first,CompBinExp(first,BinOp.*,second).setType(Type.FloatType))
      case Opcode.DIV_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        Assign(first,CompBinExp(first,BinOp./,second).setType(Type.FloatType))
      case Opcode.REM_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.FloatType)
        val second = getRegVar(binary.getRegisterB).setType(Type.FloatType)
        Assign(first,CompBinExp(first,BinOp.%,second).setType(Type.FloatType))
      case Opcode.ADD_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        Assign(first,CompBinExp(first,BinOp.+,second).setType(Type.DoubleType))
      case Opcode.SUB_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        Assign(first,CompBinExp(first,BinOp.-,second).setType(Type.DoubleType))
      case Opcode.MUL_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        Assign(first,CompBinExp(first,BinOp.*,second).setType(Type.DoubleType))
      case Opcode.DIV_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        Assign(first,CompBinExp(first,BinOp./,second).setType(Type.DoubleType))
      case Opcode.REM_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA).setType(Type.DoubleType)
        val second = getRegVar(binary.getRegisterB).setType(Type.DoubleType)
        Assign(first,CompBinExp(first,BinOp.%,second).setType(Type.DoubleType))
      case Opcode.ADD_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.+,second).setType(Type.IntegerType))
      case Opcode.RSUB_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(second,BinOp.-,first).setType(Type.IntegerType))
      case Opcode.MUL_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.*,second).setType(Type.IntegerType))
      case Opcode.DIV_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp./,second).setType(Type.IntegerType))
      case Opcode.REM_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.%,second).setType(Type.IntegerType))
      case Opcode.AND_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.&,second).setType(Type.IntegerType))
      case Opcode.OR_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.|,second).setType(Type.IntegerType))
      case Opcode.XOR_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.^,second).setType(Type.IntegerType))
      case Opcode.ADD_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.+,second).setType(Type.IntegerType))
      case Opcode.RSUB_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(second,BinOp.-,first).setType(Type.IntegerType))
      case Opcode.MUL_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.*,second).setType(Type.IntegerType))
      case Opcode.DIV_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp./,second).setType(Type.IntegerType))
      case Opcode.REM_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.%,second).setType(Type.IntegerType))
      case Opcode.AND_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.&,second).setType(Type.IntegerType))
      case Opcode.OR_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.|,second).setType(Type.IntegerType))
      case Opcode.XOR_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.^,second).setType(Type.IntegerType))
      case Opcode.SHL_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.<<,second).setType(Type.IntegerType))
      case Opcode.SHR_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.>>,second).setType(Type.IntegerType))
      case Opcode.USHR_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA).setType(Type.IntegerType)
        val first = getRegVar(binary.getRegisterB).setType(Type.IntegerType)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.>>>,second).setType(Type.IntegerType))

      case Opcode.IGET_VOLATILE |
           Opcode.IPUT_VOLATILE |
           Opcode.SGET_VOLATILE |
           Opcode.SPUT_VOLATILE |
           Opcode.IGET_OBJECT_VOLATILE |
           Opcode.IGET_WIDE_VOLATILE |
           Opcode.IPUT_WIDE_VOLATILE |
           Opcode.SGET_WIDE_VOLATILE |
           Opcode.SPUT_WIDE_VOLATILE |
           Opcode.THROW_VERIFICATION_ERROR |
           Opcode.EXECUTE_INLINE |
           Opcode.EXECUTE_INLINE_RANGE |
           Opcode.INVOKE_DIRECT_EMPTY |
           Opcode.INVOKE_OBJECT_INIT_RANGE |
           Opcode.RETURN_VOID_BARRIER |
           Opcode.IGET_QUICK |
           Opcode.IGET_WIDE_QUICK |
           Opcode.IGET_OBJECT_QUICK |
           Opcode.IPUT_QUICK |
           Opcode.IPUT_WIDE_QUICK |
           Opcode.IPUT_OBJECT_QUICK |
           Opcode.INVOKE_VIRTUAL_QUICK |
           Opcode.INVOKE_VIRTUAL_QUICK_RANGE |
           Opcode.INVOKE_SUPER_QUICK |
           Opcode.INVOKE_SUPER_QUICK_RANGE |
           Opcode.IPUT_OBJECT_VOLATILE |
           Opcode.SGET_OBJECT_VOLATILE |
           Opcode.SPUT_OBJECT_VOLATILE =>
        throw new NotSupportedException("odex not supported")

      case Opcode.PACKED_SWITCH_PAYLOAD | Opcode.SPARSE_SWITCH_PAYLOAD | Opcode.ARRAY_PAYLOAD => Nop()
    }
  }
  def methodTransform(method: DexBackedMethod) : Method = {
    val name = method.getName
    val className = ClassName(method.getDefiningClass)
    val params = method.getParameterTypes.asScala.map{typeTransform}.toList
    val sig = MethodSig(className,name,params)

    Option(method.getImplementation).map{ impl =>

      val debugInfos = impl.getDebugItems.asScala.toList // TODO : annotate opcodes by making state machine

      val instrs = impl.getInstructions.asScala.toList
      val offsetMap = buildOffsetMap(instrs)
      implicit val context = Context(offsetMap, offsetMap.map{_.swap}, instrs)
      val rawStmts = instrs.zipWithIndex.map{instructionTransform}

      def postProcess(stmts: List[Stmt]) = {
        val firstPassMap : Map[Stmt,Stmt] = instrs.map{Placeholder}.zip(stmts).toMap
        val firstPass = CommonILHelper.stmtSubstitute(firstPassMap)(stmts)
        val secondPassMap = stmts.zip(firstPass).toMap
        val secondPass = CommonILHelper.stmtSubstitute(secondPassMap)(firstPass)
        CommonILHelper.expandStmts(secondPass)
      }
      val stmts = postProcess(rawStmts)

      val regCount = impl.getRegisterCount
      val paramSizeList = params.map{CommonILHelper.getUnitSizeOf}
      val isStatic = AccessFlags.STATIC.isSet(method.getAccessFlags)
      val startingRegister = regCount - paramSizeList.reduceOption{_+_}.getOrElse(0)
      val thisStmts =
        if(!isStatic) List(Assign(getRegVar(startingRegister-1).setType(Type.RefType(className)),This.setType(Type.RefType(className))))
        else List.empty[CoreStmt]
      val paramStmts = paramSizeList.zip(params).foldLeft(thisStmts, startingRegister, 0) {
        case ((stmt,regN,count), (size,paramTy)) =>
          val regVar = getRegVar(regN).setType(paramTy)
          (Assign(regVar,Param(count).setType(paramTy))::stmt,regN+size,count+1)
      }._1.reverse
      val finalStmts = paramStmts++stmts

      Method(sig,finalStmts)

    }.getOrElse(Method(sig,List()))
  }
  def classTransform(clazz: DexBackedClassDef) : Clazz = {
    val name = ClassName(clazz.getType)
    val interfaces = clazz.getInterfaces.asScala.map{ClassName.apply}.toSet
    val superClass = ClassName(clazz.getSuperclass)

    val methods = clazz.getMethods.asScala
    val methodMap = methods.map{methodTransform}.map{x=>(x.name,x)}.toMap

    Clazz(name, methodMap, interfaces, superClass)
  }
  def translate(dexFile: DexBackedDexFile) : Program = {
    val classSet = dexFile.getClasses.asScala
    val classMap = classSet.map{classTransform}.map{x=>(x.name,x)}.toMap
    Program(classMap)
  }
  private def removeSecondOfWideRegisters(paramTypes: List[Type.ValueType], args: List[Value.t]) : List[Value.t] = {
    paramTypes.map{CommonILHelper.getUnitSizeOf}.flatMap{case 1 => List(1); case 2 => List(1,0)}.zip(args).flatMap{case (mark,v) => if(mark == 1) Some(v) else None}
  }
  private def offsetToIndex(currentIdx: Int, offset: Int)(implicit context: Context) : Int = {
    val targetOffset = context.indexToOffSetMap(currentIdx) + offset
    context.offsetToIndexMap(targetOffset)
  }
  private def buildOffsetMap(instrs: List[Instruction]) : Map[Int,Int] = {
    instrs.foldLeft(Map.empty[Int,Int],0,0) {
      case ((m,idx,offset),instr) =>
        val unitSize = instr.getCodeUnits
        (m + (offset -> idx), idx + 1, offset + unitSize)
    }._1
  }
  private def getRegVar(id: Int) = Local("$reg"+id)
  private def methodReturnVar() = Local("$mres")
}

object DexlibDexTransformer {
  class NotSupportedException(msg: String) extends Exception

  case class Context(
    offsetToIndexMap : Map[Int,Int], /* map from codeOffset to instruction list index */
    indexToOffSetMap : Map[Int,Int],
    instrs : List[Instruction] /* list of all instructions */
  )
}
