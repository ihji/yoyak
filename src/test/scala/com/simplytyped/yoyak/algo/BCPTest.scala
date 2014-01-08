package com.simplytyped.yoyak.algo

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.simplytyped.yoyak.domain._

/**
 * Created with IntelliJ IDEA.
 * User: ihji
 * Date: 5/13/12
 * Time: 8:57 PM
 * To change this template use File | Settings | File Templates.
 */

class BCPTest extends FunSuite with ShouldMatchers {
  test("simple BCP") {
    import com.simplytyped.yoyak.domain.CNFConversions._
    val cnf : CNF =
      """p cnf 3 3
        |1 0
        |1 2 0
        |-1 3 0
      """.stripMargin
    val bcp = new BCP(cnf)
    val cnf$ = bcp.run(PAssign.empty)
    cnf$ should be (PAssign(MapDom(Map(1 -> T, 3 -> T))))
  }
}
