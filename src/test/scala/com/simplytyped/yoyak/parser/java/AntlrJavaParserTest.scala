package com.simplytyped.yoyak.parser.java

import java.io.{ByteArrayInputStream, InputStream}

import org.scalatest.{Matchers, FunSuite}

class AntlrJavaParserTest extends FunSuite with Matchers {
  import AntlrTestHelper._
  test("can parser basic java file") {
    val parser = new AntlrJavaParser
    val unit   = parser.parse(toStream(simple))

    unit.toStringTree(parser.rawParser.get) should be(
      "(compilationUnit (typeDeclaration (classDeclaration class Test (classBody { (classBodyDeclaration (memberDeclaration (methodDeclaration void foo (formalParameters ( )) (methodBody (block { (blockStatement (statement (statementExpression (expression (expression (expression (expression (primary System)) . out) . println) ( (expressionList (expression (primary (literal \"hello\")))) ))) ;)) }))))) }))) <EOF>)"
    )
  }
}
