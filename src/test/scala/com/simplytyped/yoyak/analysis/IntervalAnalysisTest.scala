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
    if(!result.successful) println(result)
    val clazz = new CommonILTransform().transform(result.get)
    val pgm = Program(Map(clazz.name->clazz))
    val mtd = pgm.findByMethodName("bar").head
    val cfg = new CommonILToCFG().transform(mtd)
    //println(new PrettyPrinter().toDot(cfg))
    println(new PrettyPrinter().toString(mtd))

    val result2 = new IntervalAnalysis(cfg).run()

    println(result2)

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
      |     x = 0;
      |     y = 1;
      |     while (x < 100) {
      |       x = x + 1;
      |     }
      |     if(y > 0) {
      |       y = 10;
      |       return 1;
      |     } else {
      |       y = -10;
      |       return 0;
      |     }
      |   }
      | }
    """.stripMargin
}