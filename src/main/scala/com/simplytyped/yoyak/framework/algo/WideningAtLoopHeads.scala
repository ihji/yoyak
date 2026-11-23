package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.semantics.Widening
import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

trait WideningAtLoopHeads[D] extends DoWidening[D] {
  val cfg: CFG

  val loopHeads = CFG.traverse.findLoopheads(cfg).toSet
  protected def doWidening(
      widening: Widening[D]
  )(x: D, y: D, bb: BasicBlock): D = {
    if (loopHeads(bb)) {
      widening.<>(x, y)
    } else y
  }
}
