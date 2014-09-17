package com.simplytyped.yoyak.parser.java

import com.simplytyped.yoyak.il.CommonIL.Type.RefType
import com.simplytyped.yoyak.il.CommonIL._
import Statement._
import Value._
import com.simplytyped.yoyak.il.Position
import com.simplytyped.yoyak.il.Position.SourceInfo
import com.simplytyped.yoyak.parser.JavaParser._
import org.antlr.v4.runtime.Token
import scala.collection.JavaConverters._

class AntlrJavaTransformer {
  private def getPositionFromToken(t: Token) : Position = {
    new SourceInfo(t.getLine, t.getLine, t.getCharPositionInLine, t.getCharPositionInLine, t.getTokenSource.getSourceName)
  }
  private def typeContextToType(t: TypeContext) : Type.ValueType = {
    val dim = (t.getChildCount - 1) / 2
    val classOrInterfaceTy = t.classOrInterfaceType()
    val resultTy =
      if(classOrInterfaceTy != null) {
        val name = ClassName(classOrInterfaceTy.getText)
        Type.RefType(name)
      } else {
        val primTy = t.primitiveType()
        primTy.getText match {
          case "boolean" => Type.BooleanType
          case "char" => Type.CharType
          case "byte" => Type.ByteType
          case "short" => Type.ShortType
          case "int" => Type.IntegerType
          case "long" => Type.LongType
          case "float" => Type.FloatType
          case "double" => Type.DoubleType
        }
      }
    if(dim == 0) resultTy else Type.ArrayType(resultTy,dim)
  }
  private def formalParameterToIdentity(formalParam: FormalParameterContext, idx: Int) : Identity = {
    val name = formalParam.variableDeclaratorId().Identifier()
    val ty = typeContextToType(formalParam.`type`())
    Statement.Identity(Local(name.getText).setType(ty),Param(idx)).setPos(getPositionFromToken(name.getSymbol))
  }
  def blockStatementContextToStmt(blockStmtCtx: BlockStatementContext) : List[Stmt] = {
    List.empty
  }
  def methodDefToMethod(className: ClassName, methodDef: MethodDeclarationContext) : Option[Method] = {
    val methodName = methodDef.Identifier().getText

    val params = Option(methodDef.formalParameters().formalParameterList()).map{_.formalParameter().asScala.toList}.getOrElse(List.empty)
      .zipWithIndex.map{case (f,i) => formalParameterToIdentity(f,i)}
    val paramTy = params.map{_.lv.ty}

    val methodBody = methodDef.methodBody().block().blockStatement().asScala.toList.flatMap{blockStatementContextToStmt}

    val stmts = params ++ methodBody
    Some(Method(MethodSig(className,methodName,paramTy),stmts))
  }
  def classDefToClazz(classDef: ClassDeclarationContext) : Option[Clazz] = {
    val className = ClassName(classDef.Identifier().getSymbol.getText)
    val superClass = Option(classDef.`type`()).map{typeContextToType}.map{_.asInstanceOf[RefType].className}.getOrElse(ClassName("java.lang.Object"))
    val interfaces = Option(classDef.typeList()).map{_.`type`().asScala.map{typeContextToType}.map{_.asInstanceOf[RefType].className}.toSet}.getOrElse(Set.empty[ClassName])

    val memberDefs = classDef.classBody().classBodyDeclaration().asScala.map{_.memberDeclaration()}.toList
    val methodDefs = memberDefs.filter{_.methodDeclaration() != null}
    val methods = methodDefs.flatMap{x => methodDefToMethod(className,x.methodDeclaration())}

    Some(Clazz(className,methods.map{x=>(x.name,x)}.toMap,interfaces,superClass))
  }
  def compilationUnitToProgram(units: CompilationUnitContext) : Program = {
    val typeDefs = units.typeDeclaration().asScala.toList
    val classDefs = typeDefs.filter{_.classDeclaration() != null}
    val classes  = classDefs.flatMap{x => classDefToClazz(x.classDeclaration())}
    Program(classes.map{x=>(x.name,x)}.toMap)
  }
}
