package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.semantics.Widening
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait DoWidening[D] {
  protected def doWidening(
      widening: Widening[D]
  )(x: D, y: D, bb: BasicBlock): D
}
