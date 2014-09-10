package com.simplytyped.yoyak.parser.dex

import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Type
import com.simplytyped.yoyak.il.CommonIL.Value._
import com.simplytyped.yoyak.il.SourceInfo
import com.simplytyped.yoyak.parser.dex.DexlibDexTransformer.InvalidOrderException
import org.jf.dexlib2.Opcode
import org.jf.dexlib2.dexbacked.instruction._
import org.jf.dexlib2.iface.instruction.Instruction
import org.jf.dexlib2.iface.reference.{FieldReference, TypeReference, StringReference}

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
  def instructionTransform(instr: Instruction)(implicit instrs: List[Instruction]) : Stmt = {
    instr.getOpcode match {
      case Opcode.NOP =>
        Nop(SourceInfo.dummy)
      case Opcode.MOVE | Opcode.MOVE_WIDE | Opcode.MOVE_OBJECT =>
        val move = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(move.getRegisterA)
        val src = getRegVar(move.getRegisterB)
        Assign(dest,src,SourceInfo.dummy)
      case Opcode.MOVE_FROM16 | Opcode.MOVE_WIDE_FROM16 | Opcode.MOVE_OBJECT_FROM16 =>
        val move = instr.asInstanceOf[DexBackedInstruction22x]
        val dest = getRegVar(move.getRegisterA)
        val src = getRegVar(move.getRegisterB)
        Assign(dest,src,SourceInfo.dummy)
      case Opcode.MOVE_16 | Opcode.MOVE_WIDE_16 | Opcode.MOVE_OBJECT_16 =>
        val move = instr.asInstanceOf[DexBackedInstruction32x]
        val dest = getRegVar(move.getRegisterA)
        val src = getRegVar(move.getRegisterB)
        Assign(dest,src,SourceInfo.dummy)
      case Opcode.MOVE_RESULT | Opcode.MOVE_RESULT_WIDE | Opcode.MOVE_RESULT_OBJECT =>
        throw new InvalidOrderException("should be done immediately after invoke-kind")
      case Opcode.MOVE_EXCEPTION =>
        val move = instr.asInstanceOf[DexBackedInstruction11x]
        val dest = getRegVar(move.getRegisterA)
        Assign(dest,CaughtExceptionRef,SourceInfo.dummy)
      case Opcode.RETURN_VOID =>
        Return(None,SourceInfo.dummy)
      case Opcode.RETURN | Opcode.RETURN_WIDE | Opcode.RETURN_OBJECT =>
        val ret = instr.asInstanceOf[DexBackedInstruction11x]
        val retVal = getRegVar(ret.getRegisterA)
        Return(Some(retVal),SourceInfo.dummy)
      case Opcode.CONST_4 =>
        val const = instr.asInstanceOf[DexBackedInstruction11n]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getNarrowLiteral
        Assign(dest,IntegerConstant(constVal),SourceInfo.dummy)
      case Opcode.CONST_16 | Opcode.CONST_WIDE_16 =>
        val const = instr.asInstanceOf[DexBackedInstruction21s]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getNarrowLiteral
        Assign(dest,IntegerConstant(constVal),SourceInfo.dummy)
      case Opcode.CONST | Opcode.CONST_WIDE_32 =>
        val const = instr.asInstanceOf[DexBackedInstruction31i]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getNarrowLiteral
        Assign(dest,IntegerConstant(constVal),SourceInfo.dummy)
      case Opcode.CONST_HIGH16 =>
        val const = instr.asInstanceOf[DexBackedInstruction21ih]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getNarrowLiteral
        Assign(dest,IntegerConstant(constVal),SourceInfo.dummy)
      case Opcode.CONST_WIDE =>
        val const = instr.asInstanceOf[DexBackedInstruction51l]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getWideLiteral
        Assign(dest,LongConstant(constVal),SourceInfo.dummy)
      case Opcode.CONST_WIDE_HIGH16 =>
        val const = instr.asInstanceOf[DexBackedInstruction21lh]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getWideLiteral
        Assign(dest,LongConstant(constVal),SourceInfo.dummy)
      case Opcode.CONST_STRING =>
        val const = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getReference.asInstanceOf[StringReference].getString
        Assign(dest,StringConstant(constVal),SourceInfo.dummy)
      case Opcode.CONST_STRING_JUMBO =>
        val const = instr.asInstanceOf[DexBackedInstruction31c]
        val dest = getRegVar(const.getRegisterA)
        val constVal = const.getReference.asInstanceOf[StringReference].getString
        Assign(dest,StringConstant(constVal),SourceInfo.dummy)
      case Opcode.CONST_CLASS =>
        val const = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(const.getRegisterA)
        val constVal = typeTransform(const.getReference.asInstanceOf[TypeReference].getType)
        Assign(dest,ClassConstant(constVal),SourceInfo.dummy)
      case Opcode.MONITOR_ENTER =>
        val mon = instr.asInstanceOf[DexBackedInstruction11x]
        val loc = getRegVar(mon.getRegisterA)
        EnterMonitor(loc,SourceInfo.dummy)
      case Opcode.MONITOR_EXIT =>
        val mon = instr.asInstanceOf[DexBackedInstruction11x]
        val loc = getRegVar(mon.getRegisterA)
        ExitMonitor(loc,SourceInfo.dummy)
      case Opcode.CHECK_CAST =>
        val check = instr.asInstanceOf[DexBackedInstruction21c]
        val loc = getRegVar(check.getRegisterA)
        val ty = typeTransform(check.getReference.asInstanceOf[TypeReference].getType)
        Assign(loc,CastExp(loc,ty),SourceInfo.dummy)
      case Opcode.INSTANCE_OF =>
        val instanceOf = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(instanceOf.getRegisterA)
        val loc = getRegVar(instanceOf.getRegisterB)
        val ty = typeTransform(instanceOf.getReference.asInstanceOf[TypeReference].getType)
        Assign(dest,InstanceOfExp(loc,ty),SourceInfo.dummy)
      case Opcode.ARRAY_LENGTH =>
        val length = instr.asInstanceOf[DexBackedInstruction12x]
        val dest = getRegVar(length.getRegisterA)
        val loc = getRegVar(length.getRegisterB)
        Assign(dest,LengthExp(loc),SourceInfo.dummy)
      case Opcode.NEW_INSTANCE =>
        val newInstance = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(newInstance.getRegisterA)
        val ty = typeTransform(newInstance.getReference.asInstanceOf[TypeReference].getType)
        Assign(dest,NewExp(ty),SourceInfo.dummy)
      case Opcode.NEW_ARRAY =>
        val newArray = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(newArray.getRegisterA)
        val size = getRegVar(newArray.getRegisterB)
        val ty = typeTransform(newArray.getReference.asInstanceOf[TypeReference].getType)
        Assign(dest,NewArrayExp(ty,size),SourceInfo.dummy)
      case Opcode.FILLED_NEW_ARRAY => ???
      case Opcode.FILLED_NEW_ARRAY_RANGE => ???
      case Opcode.FILL_ARRAY_DATA => ???
      case Opcode.THROW =>
        val thr = instr.asInstanceOf[DexBackedInstruction11x]
        val loc = getRegVar(thr.getRegisterA)
        Throw(loc,SourceInfo.dummy)
      case Opcode.GOTO =>
        val goto = instr.asInstanceOf[DexBackedInstruction10t]
        val target = goto.getCodeOffset
        Goto(target,SourceInfo.dummy)
      case Opcode.GOTO_16 =>
        val goto = instr.asInstanceOf[DexBackedInstruction20t]
        val target = goto.getCodeOffset
        Goto(target,SourceInfo.dummy)
      case Opcode.GOTO_32 =>
        val goto = instr.asInstanceOf[DexBackedInstruction30t]
        val target = goto.getCodeOffset
        Goto(target,SourceInfo.dummy)
      case Opcode.PACKED_SWITCH =>
        val switch = instr.asInstanceOf[DexBackedInstruction31t]
        val test = getRegVar(switch.getRegisterA)
        val payload = instrs(switch.getCodeOffset).asInstanceOf[DexBackedPackedSwitchPayload]
        val table = payload.getSwitchElements.asScala.toList
        val revLists = table.foldLeft(List.empty[Int], List.empty[Int]){ case ((ks,vs),x) => (x.getKey::ks,x.getOffset::vs) }
        val keys = revLists._1.reverse.map{IntegerConstant}
        val offsets = revLists._2.reverse
        Switch(test,keys,offsets,SourceInfo.dummy)
      case Opcode.SPARSE_SWITCH =>
        val switch = instr.asInstanceOf[DexBackedInstruction31t]
        val test = getRegVar(switch.getRegisterA)
        val payload = instrs(switch.getCodeOffset).asInstanceOf[DexBackedSparseSwitchPayload]
        val table = payload.getSwitchElements.asScala.toList
        val revLists = table.foldLeft(List.empty[Int], List.empty[Int]){ case ((ks,vs),x) => (x.getKey::ks,x.getOffset::vs) }
        val keys = revLists._1.reverse.map{IntegerConstant}
        val offsets = revLists._2.reverse
        Switch(test,keys,offsets,SourceInfo.dummy)
      case Opcode.CMPL_FLOAT =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmpl,second),SourceInfo.dummy)
      case Opcode.CMPG_FLOAT =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmpg,second),SourceInfo.dummy)
      case Opcode.CMPL_DOUBLE =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmpl,second),SourceInfo.dummy)
      case Opcode.CMPG_DOUBLE =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmpg,second),SourceInfo.dummy)
      case Opcode.CMP_LONG =>
        val cmp = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(cmp.getRegisterA)
        val first = getRegVar(cmp.getRegisterB)
        val second = getRegVar(cmp.getRegisterC)
        Assign(dest,CompBinExp(first,BinOp.cmp,second),SourceInfo.dummy)
      case Opcode.IF_EQ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(first,BinOp.==,second),offset,SourceInfo.dummy)
      case Opcode.IF_NE =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(first,BinOp.!=,second),offset,SourceInfo.dummy)
      case Opcode.IF_LT =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(first,BinOp.<,second),offset,SourceInfo.dummy)
      case Opcode.IF_GE =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(first,BinOp.>=,second),offset,SourceInfo.dummy)
      case Opcode.IF_GT =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(first,BinOp.>,second),offset,SourceInfo.dummy)
      case Opcode.IF_LE =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction22t]
        val first = getRegVar(ifcond.getRegisterA)
        val second = getRegVar(ifcond.getRegisterB)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(first,BinOp.<=,second),offset,SourceInfo.dummy)
      case Opcode.IF_EQZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(test,BinOp.==,IntegerConstant(0)),offset,SourceInfo.dummy)
      case Opcode.IF_NEZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(test,BinOp.!=,IntegerConstant(0)),offset,SourceInfo.dummy)
      case Opcode.IF_LTZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(test,BinOp.<,IntegerConstant(0)),offset,SourceInfo.dummy)
      case Opcode.IF_GEZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(test,BinOp.>=,IntegerConstant(0)),offset,SourceInfo.dummy)
      case Opcode.IF_GTZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(test,BinOp.>,IntegerConstant(0)),offset,SourceInfo.dummy)
      case Opcode.IF_LEZ =>
        val ifcond = instr.asInstanceOf[DexBackedInstruction21t]
        val test = getRegVar(ifcond.getRegisterA)
        val offset = ifcond.getCodeOffset
        If(CondBinExp(test,BinOp.<=,IntegerConstant(0)),offset,SourceInfo.dummy)
      case Opcode.AGET =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index),SourceInfo.dummy)
      case Opcode.AGET_WIDE =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index),SourceInfo.dummy)
      case Opcode.AGET_OBJECT =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index),SourceInfo.dummy)
      case Opcode.AGET_BOOLEAN =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index),SourceInfo.dummy)
      case Opcode.AGET_BYTE =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index),SourceInfo.dummy)
      case Opcode.AGET_CHAR =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index),SourceInfo.dummy)
      case Opcode.AGET_SHORT =>
        val get = instr.asInstanceOf[DexBackedInstruction23x]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val index = getRegVar(get.getRegisterC)
        Assign(dest,ArrayRef(base,index),SourceInfo.dummy)
      case Opcode.APUT =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src,SourceInfo.dummy)
      case Opcode.APUT_WIDE =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src,SourceInfo.dummy)
      case Opcode.APUT_OBJECT =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src,SourceInfo.dummy)
      case Opcode.APUT_BOOLEAN =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src,SourceInfo.dummy)
      case Opcode.APUT_BYTE =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src,SourceInfo.dummy)
      case Opcode.APUT_CHAR =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src,SourceInfo.dummy)
      case Opcode.APUT_SHORT =>
        val put = instr.asInstanceOf[DexBackedInstruction23x]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val index = getRegVar(put.getRegisterC)
        Assign(ArrayRef(base,index),src,SourceInfo.dummy)
      case Opcode.IGET =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field),SourceInfo.dummy)
      case Opcode.IGET_WIDE =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field),SourceInfo.dummy)
      case Opcode.IGET_OBJECT =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field),SourceInfo.dummy)
      case Opcode.IGET_BOOLEAN =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field),SourceInfo.dummy)
      case Opcode.IGET_BYTE =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field),SourceInfo.dummy)
      case Opcode.IGET_CHAR =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field),SourceInfo.dummy)
      case Opcode.IGET_SHORT =>
        val get = instr.asInstanceOf[DexBackedInstruction22c]
        val dest = getRegVar(get.getRegisterA)
        val base = getRegVar(get.getRegisterB)
        val field = get.getReference.asInstanceOf[FieldReference].getName
        Assign(dest,InstanceFieldRef(base,field),SourceInfo.dummy)
      case Opcode.IPUT =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src,SourceInfo.dummy)
      case Opcode.IPUT_WIDE =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src,SourceInfo.dummy)
      case Opcode.IPUT_OBJECT =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src,SourceInfo.dummy)
      case Opcode.IPUT_BOOLEAN =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src,SourceInfo.dummy)
      case Opcode.IPUT_BYTE =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src,SourceInfo.dummy)
      case Opcode.IPUT_CHAR =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src,SourceInfo.dummy)
      case Opcode.IPUT_SHORT =>
        val put = instr.asInstanceOf[DexBackedInstruction22c]
        val src = getRegVar(put.getRegisterA)
        val base = getRegVar(put.getRegisterB)
        val field = put.getReference.asInstanceOf[FieldReference].getName
        Assign(InstanceFieldRef(base,field),src,SourceInfo.dummy)
      case Opcode.SGET =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName),SourceInfo.dummy)
      case Opcode.SGET_WIDE =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName),SourceInfo.dummy)
      case Opcode.SGET_OBJECT =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName),SourceInfo.dummy)
      case Opcode.SGET_BOOLEAN =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName),SourceInfo.dummy)
      case Opcode.SGET_BYTE =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName),SourceInfo.dummy)
      case Opcode.SGET_CHAR =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName),SourceInfo.dummy)
      case Opcode.SGET_SHORT =>
        val get = instr.asInstanceOf[DexBackedInstruction21c]
        val dest = getRegVar(get.getRegisterA)
        val fieldRef = get.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(dest,StaticFieldRef(className,fieldName),SourceInfo.dummy)
      case Opcode.SPUT =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src,SourceInfo.dummy)
      case Opcode.SPUT_WIDE =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src,SourceInfo.dummy)
      case Opcode.SPUT_OBJECT =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src,SourceInfo.dummy)
      case Opcode.SPUT_BOOLEAN =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src,SourceInfo.dummy)
      case Opcode.SPUT_BYTE =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src,SourceInfo.dummy)
      case Opcode.SPUT_CHAR =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src,SourceInfo.dummy)
      case Opcode.SPUT_SHORT =>
        val put = instr.asInstanceOf[DexBackedInstruction21c]
        val src = getRegVar(put.getRegisterA)
        val fieldRef = put.getReference.asInstanceOf[FieldReference]
        val className = ClassName(fieldRef.getDefiningClass)
        val fieldName = fieldRef.getName
        Assign(StaticFieldRef(className,fieldName),src,SourceInfo.dummy)
      case Opcode.INVOKE_VIRTUAL => ???
      case Opcode.INVOKE_SUPER => ???
      case Opcode.INVOKE_DIRECT => ???
      case Opcode.INVOKE_STATIC => ???
      case Opcode.INVOKE_INTERFACE => ???
      case Opcode.INVOKE_VIRTUAL_RANGE => ???
      case Opcode.INVOKE_SUPER_RANGE => ???
      case Opcode.INVOKE_DIRECT_RANGE => ???
      case Opcode.INVOKE_STATIC_RANGE => ???
      case Opcode.INVOKE_INTERFACE_RANGE => ???
      case Opcode.NEG_INT => ???
      case Opcode.NOT_INT => ???
      case Opcode.NEG_LONG => ???
      case Opcode.NOT_LONG => ???
      case Opcode.NEG_FLOAT => ???
      case Opcode.NEG_DOUBLE => ???
      case Opcode.INT_TO_LONG => ???
      case Opcode.INT_TO_FLOAT => ???
      case Opcode.INT_TO_DOUBLE => ???
      case Opcode.LONG_TO_INT => ???
      case Opcode.LONG_TO_FLOAT => ???
      case Opcode.LONG_TO_DOUBLE => ???
      case Opcode.FLOAT_TO_INT => ???
      case Opcode.FLOAT_TO_LONG => ???
      case Opcode.FLOAT_TO_DOUBLE => ???
      case Opcode.DOUBLE_TO_INT => ???
      case Opcode.DOUBLE_TO_LONG => ???
      case Opcode.DOUBLE_TO_FLOAT => ???
      case Opcode.INT_TO_BYTE => ???
      case Opcode.INT_TO_CHAR => ???
      case Opcode.INT_TO_SHORT => ???
      case Opcode.ADD_INT => ???
      case Opcode.SUB_INT => ???
      case Opcode.MUL_INT => ???
      case Opcode.DIV_INT => ???
      case Opcode.REM_INT => ???
      case Opcode.AND_INT => ???
      case Opcode.OR_INT => ???
      case Opcode.XOR_INT => ???
      case Opcode.SHL_INT => ???
      case Opcode.SHR_INT => ???
      case Opcode.USHR_INT => ???
      case Opcode.ADD_LONG => ???
      case Opcode.SUB_LONG => ???
      case Opcode.MUL_LONG => ???
      case Opcode.DIV_LONG => ???
      case Opcode.REM_LONG => ???
      case Opcode.AND_LONG => ???
      case Opcode.OR_LONG => ???
      case Opcode.XOR_LONG => ???
      case Opcode.SHL_LONG => ???
      case Opcode.SHR_LONG => ???
      case Opcode.USHR_LONG => ???
      case Opcode.ADD_FLOAT => ???
      case Opcode.SUB_FLOAT => ???
      case Opcode.MUL_FLOAT => ???
      case Opcode.DIV_FLOAT => ???
      case Opcode.REM_FLOAT => ???
      case Opcode.ADD_DOUBLE => ???
      case Opcode.SUB_DOUBLE => ???
      case Opcode.MUL_DOUBLE => ???
      case Opcode.DIV_DOUBLE => ???
      case Opcode.REM_DOUBLE => ???
      case Opcode.ADD_INT_2ADDR => ???
      case Opcode.SUB_INT_2ADDR => ???
      case Opcode.MUL_INT_2ADDR => ???
      case Opcode.DIV_INT_2ADDR => ???
      case Opcode.REM_INT_2ADDR => ???
      case Opcode.AND_INT_2ADDR => ???
      case Opcode.OR_INT_2ADDR => ???
      case Opcode.XOR_INT_2ADDR => ???
      case Opcode.SHL_INT_2ADDR => ???
      case Opcode.SHR_INT_2ADDR => ???
      case Opcode.USHR_INT_2ADDR => ???
      case Opcode.ADD_LONG_2ADDR => ???
      case Opcode.SUB_LONG_2ADDR => ???
      case Opcode.MUL_LONG_2ADDR => ???
      case Opcode.DIV_LONG_2ADDR => ???
      case Opcode.REM_LONG_2ADDR => ???
      case Opcode.AND_LONG_2ADDR => ???
      case Opcode.OR_LONG_2ADDR => ???
      case Opcode.XOR_LONG_2ADDR => ???
      case Opcode.SHL_LONG_2ADDR => ???
      case Opcode.SHR_LONG_2ADDR => ???
      case Opcode.USHR_LONG_2ADDR => ???
      case Opcode.ADD_FLOAT_2ADDR => ???
      case Opcode.SUB_FLOAT_2ADDR => ???
      case Opcode.MUL_FLOAT_2ADDR => ???
      case Opcode.DIV_FLOAT_2ADDR => ???
      case Opcode.REM_FLOAT_2ADDR => ???
      case Opcode.ADD_DOUBLE_2ADDR => ???
      case Opcode.SUB_DOUBLE_2ADDR => ???
      case Opcode.MUL_DOUBLE_2ADDR => ???
      case Opcode.DIV_DOUBLE_2ADDR => ???
      case Opcode.REM_DOUBLE_2ADDR => ???
      case Opcode.ADD_INT_LIT16 => ???
      case Opcode.RSUB_INT => ???
      case Opcode.MUL_INT_LIT16 => ???
      case Opcode.DIV_INT_LIT16 => ???
      case Opcode.REM_INT_LIT16 => ???
      case Opcode.AND_INT_LIT16 => ???
      case Opcode.OR_INT_LIT16 => ???
      case Opcode.XOR_INT_LIT16 => ???
      case Opcode.ADD_INT_LIT8 => ???
      case Opcode.RSUB_INT_LIT8 => ???
      case Opcode.MUL_INT_LIT8 => ???
      case Opcode.DIV_INT_LIT8 => ???
      case Opcode.REM_INT_LIT8 => ???
      case Opcode.AND_INT_LIT8 => ???
      case Opcode.OR_INT_LIT8 => ???
      case Opcode.XOR_INT_LIT8 => ???
      case Opcode.SHL_INT_LIT8 => ???
      case Opcode.SHR_INT_LIT8 => ???
      case Opcode.USHR_INT_LIT8 => ???

      case Opcode.IGET_VOLATILE => ???
      case Opcode.IPUT_VOLATILE => ???
      case Opcode.SGET_VOLATILE => ???
      case Opcode.SPUT_VOLATILE => ???
      case Opcode.IGET_OBJECT_VOLATILE => ???
      case Opcode.IGET_WIDE_VOLATILE => ???
      case Opcode.IPUT_WIDE_VOLATILE => ???
      case Opcode.SGET_WIDE_VOLATILE => ???
      case Opcode.SPUT_WIDE_VOLATILE => ???

      case Opcode.THROW_VERIFICATION_ERROR => ???
      case Opcode.EXECUTE_INLINE => ???
      case Opcode.EXECUTE_INLINE_RANGE => ???
      case Opcode.INVOKE_DIRECT_EMPTY => ???
      case Opcode.INVOKE_OBJECT_INIT_RANGE => ???
      case Opcode.RETURN_VOID_BARRIER => ???
      case Opcode.IGET_QUICK => ???
      case Opcode.IGET_WIDE_QUICK => ???
      case Opcode.IGET_OBJECT_QUICK => ???
      case Opcode.IPUT_QUICK => ???
      case Opcode.IPUT_WIDE_QUICK => ???
      case Opcode.IPUT_OBJECT_QUICK => ???
      case Opcode.INVOKE_VIRTUAL_QUICK => ???
      case Opcode.INVOKE_VIRTUAL_QUICK_RANGE => ???
      case Opcode.INVOKE_SUPER_QUICK => ???
      case Opcode.INVOKE_SUPER_QUICK_RANGE => ???

      case Opcode.IPUT_OBJECT_VOLATILE => ???
      case Opcode.SGET_OBJECT_VOLATILE => ???
      case Opcode.SPUT_OBJECT_VOLATILE => ???

      case Opcode.PACKED_SWITCH_PAYLOAD | Opcode.SPARSE_SWITCH_PAYLOAD | Opcode.ARRAY_PAYLOAD =>
        throw new InvalidOrderException("should be done as payload")
    }
  }
  def methodTransform(method: DexBackedMethod) : Method = {
    val name = method.getName
    val className = ClassName(method.getDefiningClass)
    val params = method.getParameterTypes.asScala.map{typeTransform}.toList
    val sig = MethodSig(className,name,params)

    val debugInfos = method.getImplementation.getDebugItems.asScala.toList // TODO : annotate opcodes by making state machine
    implicit val instrs = method.getImplementation.getInstructions.asScala.toList
    val stmts = instrs.map{instructionTransform}

    Method(sig,stmts)
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
  private def getRegVar(id: Int) = Local("$reg"+id,Type.UnknownType)
}

object DexlibDexTransformer {
  class InvalidOrderException(msg: String) extends Exception
}