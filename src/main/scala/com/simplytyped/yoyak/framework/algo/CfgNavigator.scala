package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.{Galois, MapDom}
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait CfgNavigator[D <: Galois] {
  def getNextBlocks(bb: BasicBlock): Seq[BasicBlock]
  def memoryFetcher(map: MapDom[BasicBlock, D], b: BasicBlock): Seq[D#Abst]
}
