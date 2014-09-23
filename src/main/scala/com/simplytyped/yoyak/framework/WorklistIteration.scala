package com.simplytyped.yoyak.framework

import com.simplytyped.yoyak.il.cfg.{BasicBlock, CFG}

class WorklistIteration[D](cfg: CFG) {
  var worklist = Worklist.empty[BasicBlock]
  def computeFixedPoint(input: D, startNodes: List[BasicBlock]) : D = {
    ???
  }
}
