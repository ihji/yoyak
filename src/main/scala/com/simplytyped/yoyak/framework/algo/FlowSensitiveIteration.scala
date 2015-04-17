package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.MapDom
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait FlowSensitiveIteration[D] {
  implicit val absTransfer : AbstractTransferable[D]

  protected def work(map: MapDom[BasicBlock,D], input: D, block: BasicBlock) : (MapDom[BasicBlock,D], D) = {
    val stmts = block.data.getStmts
    val output = stmts.foldLeft(input){absTransfer.transfer}
    (map,output)
  }
}
