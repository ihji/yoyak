package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.{Galois, LatticeOps}
import com.simplytyped.yoyak.framework.semantics.Widening
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait FlowInsensitiveFixedPointComputation[D<:Galois] extends FlowInsensitiveIteration[D] with DoWidening[D] {
  implicit val ops : LatticeOps[D]
  def getNextBlocks(bb: BasicBlock) : Seq[BasicBlock]

  val worklist = Worklist.empty[BasicBlock]

  def computeFixedPoint(input: D#Abst, startNodes: List[BasicBlock])(implicit widening: Option[Widening[D]] = None) : D#Abst = {
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
        next = if(widening.nonEmpty) {
          doWidening(widening.get)(prev,next,bb)
        } else next
        val nextWork = getNextBlocks(bb)
        worklist.add(nextWork:_*)
      }
    }
    next
  }
}
