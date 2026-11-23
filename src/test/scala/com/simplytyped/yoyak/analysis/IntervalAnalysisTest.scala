package com.simplytyped.yoyak.analysis

import com.simplytyped.yoyak.framework.domain.arith.Interval._
import com.simplytyped.yoyak.framework.domain.arith.{IntervalInt, Interv}
import com.simplytyped.yoyak.framework.domain.mem.MemElems.AbsArith
import com.simplytyped.yoyak.il.CommonIL.Statement.Return
import com.simplytyped.yoyak.il.CommonIL.Value.Local
import com.simplytyped.yoyak.il.cfg.CFG
import com.simplytyped.yoyak.il.CommonIL.Program
import com.simplytyped.yoyak.il.PrettyPrinter
import com.simplytyped.yoyak.il.cfg.CommonILToCFG
import com.simplytyped.yoyak.parser.cil.{CommonILParser, CommonILTransform}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class IntervalAnalysisTest extends AnyFunSuite with Matchers {
  private def cfgFor(body: String): CFG = {
    val parser = new CommonILParser
    val source = s"example { run(flag: int) { $body } }"
    val clazz = new CommonILTransform().transform(parser.parseAll(parser.clazz, source).get)
    val method = Program(Map(clazz.name -> clazz)).findByMethodName("run").head
    new CommonILToCFG().transform(method)
  }

  test("both branch values reach the join and feed the next assignment") {
    val cfg = cfgFor("""
      if (flag > 0) { x = 1; } else { x = 3; }
      y = x + 2;
      return y;
    """)
    val returns = cfg.nodes.filter(_.data.getStmts.exists(_.isInstanceOf[Return]))
    returns should have size 1
    val atReturn = new IntervalAnalysis(cfg).run().get(returns.head)
    atReturn.get(Local("x")) shouldBe AbsArith[IntervalInt](Interv.in(IInt(1), IInt(3)))
    atReturn.get(Local("y")) shouldBe AbsArith[IntervalInt](Interv.in(IInt(3), IInt(5)))
  }

  test("loop-head widening expands the growing bound and preserves a stable local") {
    val cfg = cfgFor("""
      x = 1;
      stable = 7;
      while (flag > 0) { x = 3; }
      return x;
    """)
    val heads = CFG.traverse.findLoopheads(cfg)
    heads should have size 1
    val atHead = new IntervalAnalysis(cfg).run().get(heads.head)
    atHead.get(Local("x")) shouldBe AbsArith[IntervalInt](Interv.in(IInt(1), IInf))
    atHead.get(Local("stable")) shouldBe AbsArith[IntervalInt](Interv.of(7))
  }

  test("simple test") {
    val parser = new CommonILParser
    val result = parser.parseAll(parser.clazz, IntervalAnalysisTest.inputPgm)
    if (!result.successful) println(result)
    val clazz = new CommonILTransform().transform(result.get)
    val pgm   = Program(Map(clazz.name -> clazz))
    val mtd   = pgm.findByMethodName("bar").head
    val cfg   = new CommonILToCFG().transform(mtd)
    //println(new PrettyPrinter().toDot(cfg))
    println(new PrettyPrinter().toString(mtd))

    val result2 = new IntervalAnalysis(cfg).run()

    println(result2)

    pgm.findByMethodName("bar").nonEmpty should be(true)
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
