package com.simplytyped.yoyak.algo

import com.simplytyped.yoyak.domain._


/**
 * Created with IntelliJ IDEA.
 * User: ihji
 * Date: 5/13/12
 * Time: 7:43 PM
 * To change this template use File | Settings | File Templates.
 */

class BCP(cnf: CNF) {
  def unit(p: PAssignDom, c: Clause) : PAssignDom = {
    p match {
      case x : PAssign =>
        val undet = c.vars.filterNot{x.get(_) == F}
        if(undet.size == 0) PAssignTop(x)
        else if(undet.size == 1 && x.get(undet.head) == Bot) x.update(undet.head,T)
        else x
      case x : PAssignTop => x
    }
  }
  def run(phi: PAssignDom) : PAssignDom = {
    var oldPhi : PAssignDom = PAssign.empty
    var newPhi : PAssignDom = phi
    do {
      oldPhi = newPhi
      newPhi = cnf.clauses.foldLeft(newPhi){unit(_,_)}
    } while(!(newPhi <<= oldPhi).get)
    newPhi
  }
}
