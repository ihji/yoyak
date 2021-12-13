package com.simplytyped.yoyak.solver.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Created with IntelliJ IDEA.
  * User: ihji
  * Date: 5/13/12
  * Time: 5:33 PM
  * To change this template use File | Settings | File Templates.
  */

class DIMACSParserTest extends AnyFunSuite with Matchers {
  test("basic DIMACS parsing") {
    val parser = new DIMACSParser {}
    val file =
      """c this is comment.
        |c this is another comment.
        |p cnf 5 3
        |1 -5 4 0
        |-1 5 3 4 0
        |-3 -4 0
      """.stripMargin
    val result = parser.parseAll(parser.cnf, file)
    result.successful should be(true)
    result.get should be(
      Some(
        CNF(
          5,
          3,
          List(
            Clause(List(1, -5, 4)),
            Clause(List(-1, 5, 3, 4)),
            Clause(List(-3, -4))
          )
        )
      )
    )
  }
  test("semantic error DIMACS parsing") {
    val parser = new DIMACSParser {}
    val file =
      """p cnf 5 3
        |1 -5 4 0
        |-1 6 3 4 0
        |-3 -4 0
      """.stripMargin
    val result = parser.parseAll(parser.cnf, file)
    result.successful should be(true)
    result.get should be(None)
  }
}
