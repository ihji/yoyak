package com.simplytyped.yoyak.analysis

import com.simplytyped.yoyak.il.CommonIL.Program
import com.simplytyped.yoyak.il.PrettyPrinter
import com.simplytyped.yoyak.il.cfg.CommonILToCFG
import com.simplytyped.yoyak.parser.cil.{CommonILParser, CommonILTransform}
import org.scalatest.{FunSuite,Matchers}

class IntervalAnalysisTest extends FunSuite with Matchers {
  test("simple test") {
    val parser = new CommonILParser
    val result = parser.parseAll(parser.clazz, IntervalAnalysisTest.inputPgm)
    val clazz = new CommonILTransform().transform(result.get)
    val pgm = Program(Map(clazz.name->clazz))
    val cfg = new CommonILToCFG().transform(pgm.findByMethodName("bar").head)
    //println(new PrettyPrinter().toDot(cfg))
    println(new PrettyPrinter().toString(pgm.findByMethodName("bar").head))

    pgm.findByMethodName("bar").nonEmpty should be (true)
  }
}

object IntervalAnalysisTest {
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