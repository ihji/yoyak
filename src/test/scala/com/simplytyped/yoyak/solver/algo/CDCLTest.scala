package com.simplytyped.yoyak.solver.algo

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import com.simplytyped.yoyak.solver.domain.{CNF, PAssign}

/** Created with IntelliJ IDEA.
  * User: ihji
  * Date: 5/14/12
  * Time: 6:16 PM
  * To change this template use File | Settings | File Templates.
  */

class CDCLTest extends AnyFunSuite with Matchers {
  test("simple CDCL") {
    import com.simplytyped.yoyak.solver.domain.CNFConversions._
    import com.simplytyped.yoyak.solver.domain.PAssign.str2PAssign
    val solution: PAssign = "110"
    val cnf: CNF =
      """p cnf 3 4
        |1 0
        |1 2 0
        |-1 2 3 0
        |-3 0
      """.stripMargin
    val cdcl = new CDCL(cnf)
    val cnf$ = cdcl.run(List(PAssign.empty))
    cnf$.get should be(solution)
  }
  test("unsat CDCL") {
    import com.simplytyped.yoyak.solver.domain.CNFConversions._
    val cnf: CNF =
      """p cnf 3 4
        |1 0
        |-1 2 0
        |-1 -2 3 0
        |-3 0
      """.stripMargin
    val cdcl = new CDCL(cnf)
    val cnf$ = cdcl.run(List(PAssign.empty))
    cnf$ should be(None)
  }
}
