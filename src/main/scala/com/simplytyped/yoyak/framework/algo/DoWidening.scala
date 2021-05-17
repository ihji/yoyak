package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.Galois
import com.simplytyped.yoyak.framework.semantics.Widening
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait DoWidening[D <: Galois] {
  protected def doWidening(
      widening: Widening[D]
  )(x: D#Abst, y: D#Abst, bb: BasicBlock): D#Abst
}
