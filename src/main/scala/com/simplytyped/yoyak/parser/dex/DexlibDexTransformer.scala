package com.simplytyped.yoyak.parser.dex

import com.simplytyped.yoyak.il.CommonIL.Statement._
import com.simplytyped.yoyak.il.CommonIL.Type
import org.jf.dexlib2.Opcode

import scala.collection.JavaConverters._
import com.simplytyped.yoyak.il.CommonIL._
import org.jf.dexlib2.dexbacked.{DexBackedMethod, DexBackedClassDef, DexBackedDexFile}

import scala.collection.mutable.ListBuffer

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
        val dim = ty.count{x => x == '['}
        val arrayTy = typeTransform(name)
        Type.ArrayType(arrayTy,dim)
      case _ => Type.RefType(ClassName(ty))
    }
  }
  def opcodeTransform(opcodes: List[Opcode]) : (List[Stmt], List[Opcode]) = {
    val opcode::remaining = opcodes
    val stmts = opcode match {
      case Opcode.NOP => ???
      case Opcode.MOVE => ???
      case Opcode.MOVE_FROM16 => ???
      case Opcode.MOVE_16 => ???
      case Opcode.MOVE_WIDE => ???
      case Opcode.MOVE_WIDE_FROM16 => ???
      case Opcode.MOVE_WIDE_16 => ???
      case Opcode.MOVE_OBJECT => ???
      case Opcode.MOVE_OBJECT_FROM16 => ???
      case Opcode.MOVE_OBJECT_16 => ???
      case Opcode.MOVE_RESULT => ???
      case Opcode.MOVE_RESULT_WIDE => ???
      case Opcode.MOVE_RESULT_OBJECT => ???
      case Opcode.MOVE_EXCEPTION => ???
      case Opcode.RETURN_VOID => ???
      case Opcode.RETURN => ???
      case Opcode.RETURN_WIDE => ???
      case Opcode.RETURN_OBJECT => ???
      case Opcode.CONST_4 => ???
      case Opcode.CONST_16 => ???
      case Opcode.CONST => ???
      case Opcode.CONST_HIGH16 => ???
      case Opcode.CONST_WIDE_16 => ???
      case Opcode.CONST_WIDE_32 => ???
      case Opcode.CONST_WIDE => ???
      case Opcode.CONST_WIDE_HIGH16 => ???
      case Opcode.CONST_STRING => ???
      case Opcode.CONST_STRING_JUMBO => ???
      case Opcode.CONST_CLASS => ???
      case Opcode.MONITOR_ENTER => ???
      case Opcode.MONITOR_EXIT => ???
      case Opcode.CHECK_CAST => ???
      case Opcode.INSTANCE_OF => ???
      case Opcode.ARRAY_LENGTH => ???
      case Opcode.NEW_INSTANCE => ???
      case Opcode.NEW_ARRAY => ???
      case Opcode.FILLED_NEW_ARRAY => ???
      case Opcode.FILLED_NEW_ARRAY_RANGE => ???
      case Opcode.FILL_ARRAY_DATA => ???
      case Opcode.THROW => ???
      case Opcode.GOTO => ???
      case Opcode.GOTO_16 => ???
      case Opcode.GOTO_32 => ???
      case Opcode.PACKED_SWITCH => ???
      case Opcode.SPARSE_SWITCH => ???
      case Opcode.CMPL_FLOAT => ???
      case Opcode.CMPG_FLOAT => ???
      case Opcode.CMPL_DOUBLE => ???
      case Opcode.CMPG_DOUBLE => ???
      case Opcode.CMP_LONG => ???
      case Opcode.IF_EQ => ???
      case Opcode.IF_NE => ???
      case Opcode.IF_LT => ???
      case Opcode.IF_GE => ???
      case Opcode.IF_GT => ???
      case Opcode.IF_LE => ???
      case Opcode.IF_EQZ => ???
      case Opcode.IF_NEZ => ???
      case Opcode.IF_LTZ => ???
      case Opcode.IF_GEZ => ???
      case Opcode.IF_GTZ => ???
      case Opcode.IF_LEZ => ???
      case Opcode.AGET => ???
      case Opcode.AGET_WIDE => ???
      case Opcode.AGET_OBJECT => ???
      case Opcode.AGET_BOOLEAN => ???
      case Opcode.AGET_BYTE => ???
      case Opcode.AGET_CHAR => ???
      case Opcode.AGET_SHORT => ???
      case Opcode.APUT => ???
      case Opcode.APUT_WIDE => ???
      case Opcode.APUT_OBJECT => ???
      case Opcode.APUT_BOOLEAN => ???
      case Opcode.APUT_BYTE => ???
      case Opcode.APUT_CHAR => ???
      case Opcode.APUT_SHORT => ???
      case Opcode.IGET => ???
      case Opcode.IGET_WIDE => ???
      case Opcode.IGET_OBJECT => ???
      case Opcode.IGET_BOOLEAN => ???
      case Opcode.IGET_BYTE => ???
      case Opcode.IGET_CHAR => ???
      case Opcode.IGET_SHORT => ???
      case Opcode.IPUT => ???
      case Opcode.IPUT_WIDE => ???
      case Opcode.IPUT_OBJECT => ???
      case Opcode.IPUT_BOOLEAN => ???
      case Opcode.IPUT_BYTE => ???
      case Opcode.IPUT_CHAR => ???
      case Opcode.IPUT_SHORT => ???
      case Opcode.SGET => ???
      case Opcode.SGET_WIDE => ???
      case Opcode.SGET_OBJECT => ???
      case Opcode.SGET_BOOLEAN => ???
      case Opcode.SGET_BYTE => ???
      case Opcode.SGET_CHAR => ???
      case Opcode.SGET_SHORT => ???
      case Opcode.SPUT => ???
      case Opcode.SPUT_WIDE => ???
      case Opcode.SPUT_OBJECT => ???
      case Opcode.SPUT_BOOLEAN => ???
      case Opcode.SPUT_BYTE => ???
      case Opcode.SPUT_CHAR => ???
      case Opcode.SPUT_SHORT => ???
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

      case Opcode.PACKED_SWITCH_PAYLOAD => ???
      case Opcode.SPARSE_SWITCH_PAYLOAD => ???
      case Opcode.ARRAY_PAYLOAD => ???
    }
    (stmts,remaining)
  }
  def methodTransform(method: DexBackedMethod) : Method = {
    val name = method.getName
    val className = ClassName(method.getDefiningClass)
    val params = method.getParameterTypes.asScala.map{typeTransform}.toList
    val sig = MethodSig(className,name,params)

    val debugInfos = method.getImplementation.getDebugItems.asScala.toList // TODO : annotate opcodes by making state machine
    var opcodes = method.getImplementation.getInstructions.asScala.map{_.getOpcode}.toList
    val stmtBuffer = new ListBuffer[Stmt]
    while(opcodes.nonEmpty) {
      val (stmts,remaining) = opcodeTransform(opcodes)
      stmtBuffer.appendAll(stmts)
      opcodes = remaining
    }

    Method(sig,stmtBuffer.toList)
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
}
