package com.simplytyped.yoyak.algo

import com.simplytyped.yoyak.domain._
import annotation.tailrec


/**
 * Created with IntelliJ IDEA.
 * User: ihji
 * Date: 5/14/12
 * Time: 2:12 AM
 * To change this template use File | Settings | File Templates.
 */

class QCP(cnf: CNF) {
  val bcp = new BCP(cnf)
  def pick(p: PAssign) : Int = {
    ((1 to cnf.nbVars).toSet -- p.map.t.keySet).head
  }
  def split(l: List[PAssign]) : List[PAssign] = {
    l.foldLeft(List.empty[PAssign],true) {
      case ((newl,flag),a) =>
        if(flag && a.size < cnf.nbVars) {
          val v = pick(a)
          (a.update(v,T)::a.update(v,F)::newl,false)
        } else (a::newl,flag)
    }._1
  }
  @tailrec
  final def run(cases: List[PAssign]) : Option[PAssign] = {
    val cases$ = cases.map{bcp.run}.filterNot{_.isTop}.asInstanceOf[List[PAssign]]
    if(cases$.isEmpty) None
    else {
      val satCases = cases$.filter{_.size == cnf.nbVars}
      if(satCases.size > 0) Some(satCases.head)
      else run(split(cases$))
    }
  }
}
