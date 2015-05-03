package com.simplytyped.yoyak.parser.cil

import org.scalatest.{FunSuite, Matchers}

class CommonILParserTest extends FunSuite with Matchers {
  test("assign statement") {
    val parser = new CommonILParser
    val result = parser.parseAll(parser.clazz, CommonILParserTest.inputPgm)
    println(result)
  }
}

object CommonILParserTest {
  val inputPgm =
    """
      | testclass {
      |   foo(x: int, y: string) {
      |     a = x;
      |     b = y;
      |     bar(b);
      |     return b;
      |   }
      |   bar(a: string) {
      |     x = a;
      |     if(x > 0) {
      |       return 1;
      |     } else {
      |       return 0;
      |     }
      |   }
      | }
    """.stripMargin
}