package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.Galois
import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait FlowInsensitiveIteration[D <: Galois] {
  implicit val absTransfer: AbstractTransferable[D]

  protected def work(input: D#Abst, block: BasicBlock): D#Abst = {
    val stmts  = block.data.getStmts
    val output = stmts.foldLeft(input) { absTransfer.transfer }
    output
  }
}
