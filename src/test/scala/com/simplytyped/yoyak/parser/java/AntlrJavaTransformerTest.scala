package com.simplytyped.yoyak.parser.java

import org.scalatest.{FunSuite, Matchers}

class AntlrJavaTransformerTest extends FunSuite with Matchers {
  import AntlrTestHelper._
  test("basic transform test") {
    val parser = new AntlrJavaParser
    val unit = parser.parse(toStream(simple))

    val program = (new AntlrJavaTransformer).compilationUnitToProgram(unit)
    println(program)
  }
}