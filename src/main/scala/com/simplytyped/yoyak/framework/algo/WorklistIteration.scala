package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.LatticeOps
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait WorklistIteration[D] {
  implicit val ops : LatticeOps[D]
  implicit val absTransfer : AbstractTransferable[D]

  val worklist = Worklist.empty[BasicBlock]

  private def work(input: D, block: BasicBlock) : D = {
    val stmts = block.data.getStmts
    val output = stmts.foldLeft(input){absTransfer.transfer}
    output
  }
  def computeFixedPoint(input: D, startNodes: List[BasicBlock], getNextWork: BasicBlock => Seq[BasicBlock]) : D = {
    assert(startNodes.size > 0)
    worklist.add(startNodes:_*)
    var next = input
    while(worklist.size() > 0) {
      val bb = worklist.pop().get
      val prev = next
      next = work(prev,bb)
      if(!ops.<=(next,prev)) {
        val nextWork = getNextWork(bb)
        worklist.add(nextWork:_*)
      }
    }
    next
  }
}
