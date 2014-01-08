package com.simplytyped.yoyak.algo

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.simplytyped.yoyak.domain.{CNF, PAssign}

/**
 * Created with IntelliJ IDEA.
 * User: ihji
 * Date: 5/14/12
 * Time: 6:16 PM
 * To change this template use File | Settings | File Templates.
 */

class CDCLTest extends FunSuite with ShouldMatchers {
  test("simple CDCL") {
    import com.simplytyped.yoyak.domain.CNFConversions._
    import com.simplytyped.yoyak.domain.PAssign.str2PAssign
    val solution : PAssign = "110"
    val cnf : CNF =
      """p cnf 3 4
        |1 0
        |1 2 0
        |-1 2 3 0
        |-3 0
      """.stripMargin
    val cdcl = new CDCL(cnf)
    val cnf$ = cdcl.run(List(PAssign.empty))
    cnf$.get should be (solution)
  }
  test("unsat CDCL") {
    import com.simplytyped.yoyak.domain.CNFConversions._
    val cnf : CNF =
      """p cnf 3 4
        |1 0
        |-1 2 0
        |-1 -2 3 0
        |-3 0
      """.stripMargin
    val cdcl = new CDCL(cnf)
    val cnf$ = cdcl.run(List(PAssign.empty))
    cnf$ should be (None)
  }
}
