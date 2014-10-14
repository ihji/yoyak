package com.simplytyped.yoyak.phases

import com.simplytyped.yoyak.Global
import com.simplytyped.yoyak.il.opt.VarSplitting

class VarSplittingPhase extends Phase {
  override def run(g: Global): Global = {
    assert(g.pgm.nonEmpty)
    for((_,cls) <- g.pgm.get.classes; (_,mtd) <- cls.methods; cfg <- mtd.cfg) {
      val varsplitting = new VarSplitting
      varsplitting.run(cfg)
    }
    g
  }
}
