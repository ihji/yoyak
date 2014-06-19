package com.simplytyped.yoyak.solver.domain

import util.parsing.combinator.RegexParsers
import com.simplytyped.yoyak.util.Log

/**
 * Created with IntelliJ IDEA.
 * User: ihji
 * Date: 5/13/12
 * Time: 5:32 PM
 * To change this template use File | Settings | File Templates.
 */

trait DIMACSParser extends RegexParsers {
  def cnf : Parser[Option[CNF]] =
    rep(comment)~>definition~rep1(clause) ^^ {
      case (nbv,nbc)~cs =>
        if(cs.length != nbc) {Log.error("the number of clauses is wrong"); None}
        else if(cs.exists{_.exists{x => x > nbv || x < -nbv}}) {Log.error("var should be in [-%s,%s]".format(nbv,nbv)); None}
        else Some(CNF(nbv,nbc,cs.map{Clause(_)}))
    }

  def comment : Parser[String] = "c"~>".*".r ^^ {identity}
  def definition : Parser[(Int,Int)] = "p"~>"cnf"~>"[1-9][0-9]*".r~"[1-9][0-9]*".r ^^ {case x~y => (x.toInt, y.toInt)}
  def clause : Parser[List[Int]] = rep1("-?[1-9][0-9]*".r)<~"0" ^^ {_.map{_.toInt}}
}
