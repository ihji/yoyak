package com.simplytyped.yoyak.parser.java

import com.simplytyped.yoyak.il.CommonIL._
import com.simplytyped.yoyak.parser.JavaParser.{MethodDeclarationContext, ClassDeclarationContext, CompilationUnitContext}
import scala.collection.JavaConverters._

class AntlrJavaTransformer {
  def methodDefToMethod(methodDef: MethodDeclarationContext) : Option[Method] = {
    Some(Method(MethodSig.dummy,List()))
  }
  def classDefToClazz(classDef: ClassDeclarationContext) : Option[Clazz] = {
    val memberDefs = classDef.classBody().classBodyDeclaration().asScala.map{_.memberDeclaration()}.toList
    val methodDefs = memberDefs.filter{_.methodDeclaration() != null}
    val methods = methodDefs.flatMap{x => methodDefToMethod(x.methodDeclaration())}

    val className = classDef.Identifier().getSymbol.getText
    Some(Clazz(ClassName(className),methods.map{x=>(x.name,x)}.toMap))
  }
  def compilationUnitToProgram(units: CompilationUnitContext) : Program = {
    val typeDefs = units.typeDeclaration().asScala.toList
    val classDefs = typeDefs.filter{_.classDeclaration() != null}
    val classes  = classDefs.flatMap{x => classDefToClazz(x.classDeclaration())}
    Program(classes.map{x=>(x.name,x)}.toMap)
  }
}
