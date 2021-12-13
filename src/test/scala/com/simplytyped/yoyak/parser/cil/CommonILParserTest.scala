package com.simplytyped.yoyak.parser.cil

import com.simplytyped.yoyak.il.PrettyPrinter
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CommonILParserTest extends AnyFunSuite with Matchers {
  test("assign statement") {
    val parser = new CommonILParser
    val result = parser.parseAll(parser.clazz, CommonILParserTest.inputPgm)
    val clazz  = new CommonILTransform().transform(result.get)
    println(new PrettyPrinter().toString(clazz))
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
