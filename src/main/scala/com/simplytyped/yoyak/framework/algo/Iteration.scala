package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.semantics.AbstractTransferable
import com.simplytyped.yoyak.il.cfg.BasicBlock

trait Iteration[D] {
   implicit val absTransfer : AbstractTransferable[D]

   protected def work(input: D, block: BasicBlock) : D = {
     val stmts = block.data.getStmts
     val output = stmts.foldLeft(input){absTransfer.transfer}
     output
   }
 }
