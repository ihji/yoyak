package com.simplytyped.yoyak.domain

import scala.language.implicitConversions

/**
 * Created with IntelliJ IDEA.
 * User: ihji
 * Date: 5/13/12
 * Time: 8:59 PM
 * To change this template use File | Settings | File Templates.
 */

object CNFConversions {
  implicit def dimacs2cnf(dimacs: String) : CNF = {
    val parser = new DIMACSParser {}
    val result = parser.parseAll(parser.cnf,dimacs)
    if(result.successful) result.get.get
    else throw new Exception(result.toString)
  }
}
