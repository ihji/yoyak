package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.LatticeOps
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait FlowInsensitiveWorklistIteration[D] extends Iteration[D] {
  implicit val ops : LatticeOps[D]

  val worklist = Worklist.empty[BasicBlock]

  def computeFixedPoint(input: D, startNodes: List[BasicBlock], getNextBlocks: BasicBlock => Seq[BasicBlock]) : D = {
    assert(startNodes.size > 0)
    worklist.add(startNodes:_*)
    var next = input
    while(worklist.size() > 0) {
      val bb = worklist.pop().get
      val prev = next
      next = work(prev,bb)
      val isStableOpt = ops.<=(next,prev)
      if(isStableOpt.isEmpty) println("error: abs. transfer func. is not distributive") // XXX: abstract transfer function is not distributive. should report this error.
      if(!isStableOpt.get) {
        val nextWork = getNextBlocks(bb)
        worklist.add(nextWork:_*)
      }
    }
    next
  }
}
