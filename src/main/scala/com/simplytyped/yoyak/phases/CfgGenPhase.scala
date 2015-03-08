package com.simplytyped.yoyak.phases

import com.simplytyped.yoyak.Global
import com.simplytyped.yoyak.il.cfg.CommonILToCFG

class CfgGenPhase extends Phase {
  override def run(g: Global): Global = {
    assert(g.pgm.nonEmpty)
    val toCFG = new CommonILToCFG
    val newClasses = g.pgm.get.classes.map {
      case (cn,cl) =>
        val newMethods = cl.methods.map{
          case (sig,mtd) =>
            val cfg = toCFG.transform(mtd.statements)
            mtd.cfg = Some(cfg)
            (sig,mtd)
        }
        (cn,cl.copy(methods = newMethods))
    }
    val newPgm = g.pgm.get.copy(classes = newClasses)
    g.copy(pgm = Some(newPgm))
  }
}
