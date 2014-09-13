package com.simplytyped.yoyak.parser.dex

import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Type
import com.simplytyped.yoyak.il.CommonIL.Type.{StaticInvoke, DynamicInvoke}
import com.simplytyped.yoyak.il.CommonIL.Value._
import com.simplytyped.yoyak.parser.dex.DexlibDexTransformer._
import org.jf.dexlib2.Opcode
import org.jf.dexlib2.dexbacked.instruction._
import org.jf.dexlib2.iface.instruction.Instruction
import org.jf.dexlib2.iface.reference.{MethodReference, FieldReference, TypeReference, StringReference}

import scala.collection.JavaConverters._
import com.simplytyped.yoyak.il.CommonIL._
import org.jf.dexlib2.dexbacked.{DexBackedMethod, DexBackedClassDef, DexBackedDexFile}

class DexlibDexTransformer {
  def typeTransform(ty: String) : Type.ValueType = {
    ty match {
      case "boolean" => Type.BooleanType
      case "char" => Type.CharType
      case "byte" => Type.ByteType
      case "short" => Type.ShortType
      case "int" => Type.IntegerType
      case "long" => Type.LongType
      case "float" => Type.FloatType
      case "double" => Type.DoubleType
      case _ if ty.endsWith("[]") =>
        val name = ty.takeWhile(_ != '[')
        val dim = ty.count { x => x == '['}
        val arrayTy = typeTransform(name)
        Type.ArrayType(arrayTy, dim)
      case _ => Type.RefType(ClassName(ty))
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
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getReference.asInstanceOf[StringReference].getString
        Assign(dest,StringConstant(constVal))
      case Opcode.CONST_STRING_JUMBO =>
        val const = instr.asInstanceOf[DexBackedInstruction31c]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getReference.asInstanceOf[StringReference].getString
        Assign(dest,StringConstant(constVal))
      case Opcode.CONST_CLASS =>
        val const = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(const.getRegisterA)
        val constVal = typeTransform(const.getReference.asInstanceOf[TypeReference].getType)
        Assign(dest,ClassConstant(constVal))
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
        val loc = getRegVar(check.getRegisterA)
        val ty = typeTransform(check.getReference.asInstanceOf[TypeReference].getType)
        Assign(loc,CastExp(loc,ty))
      case Opcode.INSTANCE_OF =>
        val instanceOf = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(instanceOf.getRegisterA)
        val loc = getRegVar(instanceOf.getRegisterB)
        val ty = typeTransform(instanceOf.getReference.asInstanceOf[TypeReference].getType)
        Assign(dest,InstanceOfExp(loc,ty))
      case Opcode.ARRAY_LENGTH =>
        val length = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(length.getRegisterA)
        val loc = getRegVar(length.getRegisterB)
        Assign(dest,LengthExp(loc))
      case Opcode.NEW_INSTANCE =>
        val newInstance = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(newInstance.getRegisterA)
        val ty = typeTransform(newInstance.getReference.asInstanceOf[TypeReference].getType)
        Assign(dest,NewExp(ty))
      case Opcode.NEW_ARRAY =>
        val newArray = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(newArray.getRegisterA)
        val size = getRegVar(newArray.getRegisterB)
        val ty = typeTransform(newArray.getReference.asInstanceOf[TypeReference].getType)
        Assign(dest,NewArrayExp(ty,size))
      case Opcode.FILLED_NEW_ARRAY => ???
      case Opcode.FILLED_NEW_ARRAY_RANGE => ???
      case Opcode.FILL_ARRAY_DATA => ???
      case Opcode.THROW =>
        val thr = instr.asInstanceOf[DexBackedInstruction11x]
        val loc = getRegVar(thr.getRegisterA)
        Throw(loc)
      case Opcode.GOTO =>
        val goto = instr.asInstanceOf[DexBackedInstruction10t]
        val target = offsetToIndex(idx,goto.getCodeOffset)
        Goto(target)
      case Opcode.GOTO_16 =>
        val goto = instr.asInstanceOf[DexBackedInstruction20t]
        val target = offsetToIndex(idx,goto.getCodeOffset)
        Goto(target)
      case Opcode.GOTO_32 =>
        val goto = instr.asInstanceOf[DexBackedInstruction30t]
        val target = offsetToIndex(idx,goto.getCodeOffset)
        Goto(target)
      case Opcode.PACKED_SWITCH =>
        val switch = instr.asInstanceOf[DexBackedInstruction31t]
        val test = getRegVar(switch.getRegisterA)
        val targetIndex = offsetToIndex(idx,switch.getCodeOffset)
        val payload = context.instrs(targetIndex).asInstanceOf[DexBackedPackedSwitchPayload]
        val table = payload.getSwitchElements.asScala.toList
        val revLists = table.foldLeft(List.empty[Int], List.empty[Int]){ case ((ks,vs),x) => (x.getKey::ks,x.getOffset::vs) }
        val keys = revLists._1.reverse.map{IntegerConstant}
        val offsets = revLists._2.reverse
        Switch(test,keys,offsets)
      case Opcode.SPARSE_SWITCH =>
        val switch = instr.asInstanceOf[DexBackedInstruction31t]
        val test = getRegVar(switch.getRegisterA)
        val targetIndex = offsetToIndex(idx,switch.getCodeOffset)
        val payload = context.instrs(targetIndex).asInstanceOf[DexBackedSparseSwitchPayload]
        val table = payload.getSwitchElements.asScala.toList
        val revLists = table.foldLeft(List.empty[Int], List.empty[Int]){ case ((ks,vs),x) => (x.getKey::ks,x.getOffset::vs) }
        val keys = revLists._1.reverse.map{IntegerConstant}
        val offsets = revLists._2.reverse
        Switch(test,keys,offsets)
      case Opcode.CMPL_FLOAT =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmpl,second))
      case Opcode.CMPG_FLOAT =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmpg,second))
      case Opcode.CMPL_DOUBLE =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmpl,second))
      case Opcode.CMPG_DOUBLE =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmpg,second))
      case Opcode.CMP_LONG =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmp,second))
      case Opcode.IF_EQ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(first,BinOp.==,second),offset)
      case Opcode.IF_NE =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(first,BinOp.!=,second),offset)
      case Opcode.IF_LT =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(first,BinOp.<,second),offset)
      case Opcode.IF_GE =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(first,BinOp.>=,second),offset)
      case Opcode.IF_GT =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(first,BinOp.>,second),offset)
      case Opcode.IF_LE =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(first,BinOp.<=,second),offset)
      case Opcode.IF_EQZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(test,BinOp.==,IntegerConstant(0)),offset)
      case Opcode.IF_NEZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(test,BinOp.!=,IntegerConstant(0)),offset)
      case Opcode.IF_LTZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(test,BinOp.<,IntegerConstant(0)),offset)
      case Opcode.IF_GEZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(test,BinOp.>=,IntegerConstant(0)),offset)
      case Opcode.IF_GTZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(test,BinOp.>,IntegerConstant(0)),offset)
      case Opcode.IF_LEZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = offsetToIndex(idx,ifcond.getCodeOffset)
        If(CondBinExp(test,BinOp.<=,IntegerConstant(0)),offset)
      case Opcode.AGET =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index))
      case Opcode.AGET_WIDE =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index))
      case Opcode.AGET_OBJECT =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index))
      case Opcode.AGET_BOOLEAN =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index))
      case Opcode.AGET_BYTE =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index))
      case Opcode.AGET_CHAR =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index))
      case Opcode.AGET_SHORT =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index))
      case Opcode.APUT =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src)
      case Opcode.APUT_WIDE =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src)
      case Opcode.APUT_OBJECT =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src)
      case Opcode.APUT_BOOLEAN =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src)
      case Opcode.APUT_BYTE =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src)
      case Opcode.APUT_CHAR =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src)
      case Opcode.APUT_SHORT =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src)
      case Opcode.IGET =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field))
      case Opcode.IGET_WIDE =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field))
      case Opcode.IGET_OBJECT =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field))
      case Opcode.IGET_BOOLEAN =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field))
      case Opcode.IGET_BYTE =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field))
      case Opcode.IGET_CHAR =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field))
      case Opcode.IGET_SHORT =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field))
      case Opcode.IPUT =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src)
      case Opcode.IPUT_WIDE =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src)
      case Opcode.IPUT_OBJECT =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src)
      case Opcode.IPUT_BOOLEAN =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src)
      case Opcode.IPUT_BYTE =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src)
      case Opcode.IPUT_CHAR =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src)
      case Opcode.IPUT_SHORT =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src)
      case Opcode.SGET =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName))
      case Opcode.SGET_WIDE =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName))
      case Opcode.SGET_OBJECT =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName))
      case Opcode.SGET_BOOLEAN =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName))
      case Opcode.SGET_BYTE =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName))
      case Opcode.SGET_CHAR =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName))
      case Opcode.SGET_SHORT =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName))
      case Opcode.SPUT =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src)
      case Opcode.SPUT_WIDE =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src)
      case Opcode.SPUT_OBJECT =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src)
      case Opcode.SPUT_BOOLEAN =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src)
      case Opcode.SPUT_BYTE =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src)
      case Opcode.SPUT_CHAR =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src)
      case Opcode.SPUT_SHORT =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src)
      case Opcode.INVOKE_VIRTUAL | Opcode.INVOKE_SUPER | Opcode.INVOKE_DIRECT | Opcode.INVOKE_INTERFACE =>
        val invoke = instr.asInstanceOf[DexBackedInstruction35c]
        val method = invoke.getReference.asInstanceOf[MethodReference]

        val className = ClassName(method.getDefiningClass)
        val methodName = method.getName
        val argTypes = method.getParameterTypes.asScala.map{x=>typeTransform(x.toString)}.toList
        val methodSig = MethodSig(className,methodName,argTypes)

        val argCount = invoke.getRegisterCount
        val args = List(getRegVar(invoke.getRegisterC),getRegVar(invoke.getRegisterD),getRegVar(invoke.getRegisterE),getRegVar(invoke.getRegisterF),getRegVar(invoke.getRegisterG)).take(argCount)

        val retType = typeTransform(method.getReturnType)
        val retVar = if(retType == Type.VoidType) None else Some(methodReturnVar)

        Invoke(retVar,DynamicInvoke(methodSig,args.tail,args.head))
      case Opcode.INVOKE_STATIC =>
        val invoke = instr.asInstanceOf[DexBackedInstruction35c]
        val method = invoke.getReference.asInstanceOf[MethodReference]

        val className = ClassName(method.getDefiningClass)
        val methodName = method.getName
        val argTypes = method.getParameterTypes.asScala.map{x=>typeTransform(x.toString)}.toList
        val methodSig = MethodSig(className,methodName,argTypes)

        val argCount = invoke.getRegisterCount
        val args = List(getRegVar(invoke.getRegisterC),getRegVar(invoke.getRegisterD),getRegVar(invoke.getRegisterE),getRegVar(invoke.getRegisterF),getRegVar(invoke.getRegisterG)).take(argCount)

        val retType = typeTransform(method.getReturnType)
        val retVar = if(retType == Type.VoidType) None else Some(methodReturnVar)

        Invoke(retVar,StaticInvoke(methodSig,args))
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

        val retType = typeTransform(method.getReturnType)
        val retVar = if(retType == Type.VoidType) None else Some(methodReturnVar)

        Invoke(retVar,DynamicInvoke(methodSig,args.tail,args.head))
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

        val retType = typeTransform(method.getReturnType)
        val retVar = if(retType == Type.VoidType) None else Some(methodReturnVar)

        Invoke(retVar,StaticInvoke(methodSig,args))
      case Opcode.NEG_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CompBinExp(IntegerConstant(0),BinOp.-,src))
      case Opcode.NOT_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CompBinExp(src,BinOp.^,IntegerConstant(-1)))
      case Opcode.NEG_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CompBinExp(LongConstant(0),BinOp.-,src))
      case Opcode.NOT_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CompBinExp(src,BinOp.^,LongConstant(-1)))
      case Opcode.NEG_FLOAT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CompBinExp(FloatConstant(0),BinOp.-,src))
      case Opcode.NEG_DOUBLE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CompBinExp(DoubleConstant(0),BinOp.-,src))
      case Opcode.INT_TO_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.LongType))
      case Opcode.INT_TO_FLOAT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.FloatType))
      case Opcode.INT_TO_DOUBLE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.DoubleType))
      case Opcode.LONG_TO_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.IntegerType))
      case Opcode.LONG_TO_FLOAT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.FloatType))
      case Opcode.LONG_TO_DOUBLE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.DoubleType))
      case Opcode.FLOAT_TO_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.IntegerType))
      case Opcode.FLOAT_TO_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.LongType))
      case Opcode.FLOAT_TO_DOUBLE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.DoubleType))
      case Opcode.DOUBLE_TO_INT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.IntegerType))
      case Opcode.DOUBLE_TO_LONG =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.LongType))
      case Opcode.DOUBLE_TO_FLOAT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.FloatType))
      case Opcode.INT_TO_BYTE =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.ByteType))
      case Opcode.INT_TO_CHAR =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.CharType))
      case Opcode.INT_TO_SHORT =>
        val unary = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(unary.getRegisterA)
        val src = getRegVar(unary.getRegisterB)
        Assign(dest,CastExp(src,Type.ShortType))
      case Opcode.ADD_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.+,second))
      case Opcode.SUB_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.-,second))
      case Opcode.MUL_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp./,second))
      case Opcode.REM_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.%,second))
      case Opcode.AND_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.&,second))
      case Opcode.OR_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.|,second))
      case Opcode.XOR_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.^,second))
      case Opcode.SHL_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.<<,second))
      case Opcode.SHR_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.>>,second))
      case Opcode.USHR_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.>>>,second))
      case Opcode.ADD_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.+,second))
      case Opcode.SUB_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.-,second))
      case Opcode.MUL_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp./,second))
      case Opcode.REM_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.%,second))
      case Opcode.AND_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.&,second))
      case Opcode.OR_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.|,second))
      case Opcode.XOR_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.^,second))
      case Opcode.SHL_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.<<,second))
      case Opcode.SHR_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.>>,second))
      case Opcode.USHR_LONG =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.>>>,second))
      case Opcode.ADD_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.+,second))
      case Opcode.SUB_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.-,second))
      case Opcode.MUL_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp./,second))
      case Opcode.REM_FLOAT =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.%,second))
      case Opcode.ADD_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.+,second))
      case Opcode.SUB_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.-,second))
      case Opcode.MUL_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp./,second))
      case Opcode.REM_DOUBLE =>
        val binary = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = getRegVar(binary.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.%,second))
      case Opcode.ADD_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.+,second))
      case Opcode.SUB_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.-,second))
      case Opcode.MUL_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp./,second))
      case Opcode.REM_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.%,second))
      case Opcode.AND_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.&,second))
      case Opcode.OR_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.|,second))
      case Opcode.XOR_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.^,second))
      case Opcode.SHL_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.<<,second))
      case Opcode.SHR_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.>>,second))
      case Opcode.USHR_INT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.>>>,second))
      case Opcode.ADD_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.+,second))
      case Opcode.SUB_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.-,second))
      case Opcode.MUL_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp./,second))
      case Opcode.REM_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.%,second))
      case Opcode.AND_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.&,second))
      case Opcode.OR_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.|,second))
      case Opcode.XOR_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.^,second))
      case Opcode.SHL_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.<<,second))
      case Opcode.SHR_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.>>,second))
      case Opcode.USHR_LONG_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.>>>,second))
      case Opcode.ADD_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.+,second))
      case Opcode.SUB_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.-,second))
      case Opcode.MUL_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp./,second))
      case Opcode.REM_FLOAT_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.%,second))
      case Opcode.ADD_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.+,second))
      case Opcode.SUB_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.-,second))
      case Opcode.MUL_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp./,second))
      case Opcode.REM_DOUBLE_2ADDR =>
        val binary = instr.asInstanceOf[DexBackedInstruction12x]
        val first = getRegVar(binary.getRegisterA)
        val second = getRegVar(binary.getRegisterB)
        Assign(first,CompBinExp(first,BinOp.%,second))
      case Opcode.ADD_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.+,second))
      case Opcode.RSUB_INT =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(second,BinOp.-,first))
      case Opcode.MUL_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp./,second))
      case Opcode.REM_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.%,second))
      case Opcode.AND_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.&,second))
      case Opcode.OR_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.|,second))
      case Opcode.XOR_INT_LIT16 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22s]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.^,second))
      case Opcode.ADD_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.+,second))
      case Opcode.RSUB_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(second,BinOp.-,first))
      case Opcode.MUL_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.*,second))
      case Opcode.DIV_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp./,second))
      case Opcode.REM_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.%,second))
      case Opcode.AND_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.&,second))
      case Opcode.OR_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.|,second))
      case Opcode.XOR_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.^,second))
      case Opcode.SHL_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.<<,second))
      case Opcode.SHR_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.>>,second))
      case Opcode.USHR_INT_LIT8 =>
        val binary = instr.asInstanceOf[DexBackedInstruction22b]
        val dest = getRegVar(binary.getRegisterA)
        val first = getRegVar(binary.getRegisterB)
        val second = IntegerConstant(binary.getNarrowLiteral)
        Assign(dest,CompBinExp(first,BinOp.>>>,second))

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
      val stmts = instrs.zipWithIndex.map{instructionTransform}

      Method(sig,stmts)

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
  private def getRegVar(id: Int) = Local("$reg"+id,Type.UnknownType)
  private val methodReturnVar = Local("$mres",Type.UnknownType)
}

object DexlibDexTransformer {
  class NotSupportedException(msg: String) extends Exception

  case class Context(
    offsetToIndexMap : Map[Int,Int], /* map from codeOffset to instruction list index */
    indexToOffSetMap : Map[Int,Int],
    instrs : List[Instruction] /* list of all instructions */
  )
}
