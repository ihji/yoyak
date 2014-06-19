package com.simplytyped.yoyak.solver.domain

/**
 * Created with IntelliJ IDEA.
 * User: ihji
 * Date: 5/13/12
 * Time: 5:41 PM
 * To change this template use File | Settings | File Templates.
 */

case class CNF(nbVars: Int, nbClauses: Int, clauses: List[Clause]) {
  def addClauses(c: List[Clause]) : CNF =
    copy(nbClauses = nbClauses + c.length, clauses = c++clauses)
}

case class Clause(vars: List[Int])
