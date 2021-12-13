package com.simplytyped.yoyak.parser.java

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AntlrJavaTransformerTest extends AnyFunSuite with Matchers {
  import AntlrTestHelper._
  ignore("basic transform test") {
    val parser = new AntlrJavaParser
    val unit   = parser.parse(toStream(simpleWithParam))

    val program = (new AntlrJavaTransformer).compilationUnitToProgram(unit)
  }
}
