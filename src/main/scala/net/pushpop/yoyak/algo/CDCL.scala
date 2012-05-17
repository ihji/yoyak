package net.pushpop.yoyak.algo

import net.pushpop.yoyak.domain._
import annotation.tailrec


/**
 * Created with IntelliJ IDEA.
 * User: ihji
 * Date: 5/14/12
 * Time: 4:13 PM
 * To change this template use File | Settings | File Templates.
 */

class CDCL(cnf: CNF) {
  var ccnf: CNF = cnf
  def search(p: PAssignTop) : Clause = {
    Clause(List.empty) // ???
  }
  def prove(cases: List[PAssign]) : (List[PAssign],List[PAssignTop]) = {
    val bcp = new BCP(ccnf)
    val qcp = new QCP(ccnf)
    val (pt,pa) = cases.map{bcp.run}.partition{_.isTop}
    (qcp.split(pa.asInstanceOf[List[PAssign]]),pt.asInstanceOf[List[PAssignTop]])
  }
  @tailrec
  final def run(cases: List[PAssign]) : Option[PAssign] = {
    val (pa,pt) = prove(cases)
    if(pa.isEmpty) None
    else {
      val satCases = pa.filter{_.size == cnf.nbVars}
      if(satCases.size > 0) Some(satCases.head)
      else {
        ccnf = ccnf.addClauses(pt.map{search})
        run(pa)
      }
    }
  }
}
