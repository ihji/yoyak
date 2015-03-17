package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.MapDom
import com.simplytyped.yoyak.framework.domain.mem.{MemDomLike, Resolvable, Localizable}
import com.simplytyped.yoyak.il.CommonIL.Program
import com.simplytyped.yoyak.il.CommonIL.Statement.{Return, CoreStmt, Invoke}
import com.simplytyped.yoyak.il.cfg.BasicEdge.InterEdge
import com.simplytyped.yoyak.il.cfg.{CFG, BasicEdge, BasicBlock}

trait Interprocedural[D<:MemDomLike[_,_,D]] {
  implicit val localize : Localizable[D]
  implicit val resolve  : Resolvable[D]

  val pgm : Program
  val worklist : Worklist[BasicBlock]

  var interproceduralEdges : CFG = CFG.empty

  def drawInterproceduralEdges(cfg: CFG, block: BasicBlock, invokeStmt: Invoke, input: D) {
    val targetMethodSig = resolve.resolve(input,invokeStmt)
    val targetMethodOpt = pgm.methods.get(targetMethodSig)
    for(targetMethod <- targetMethodOpt; targetCfg <- targetMethod.cfg;
        entry <- targetCfg.getEntry; exit <- targetCfg.getExit) {

      val nextBlocks = cfg.getNexts(block)
      assert(nextBlocks.size == 1, "invoke block can only have one next block")
      val nextBlock = nextBlocks.head

      val goEdge = BasicEdge(block,entry,InterEdge(None))
      val returnEdge = BasicEdge(exit,nextBlock,InterEdge(invokeStmt.ret))

      interproceduralEdges = interproceduralEdges.addEdge(goEdge).addEdge(returnEdge)
    }
  }
  /*
  invoking procedure:
  1. resolve callees and draw inter-procedural edges
  2. remove locals
  3. put memory at callees' entries (join?)
  4. push successors of callees' entries into a worklist
  5. push intra-procedural successors of this invoke block into a worklist

  returning procedure:
  1. get a returning value and save it into a returning memory at the special location
   something like ($__ret)
  2. remove locals
  3. put the returning memory at the exit node (join?)
  4. push successors of this exit node into a worklist

  fetching input memory:
  1. get predecessors and their edge types
  2. if the edge type is inter-procedural,
  fetch the memory and
  rename the special location to the actual return variable as shown in the edge
  3. join all input memories
   */

  def evalInvoke(cfg: CFG, block: BasicBlock, stmt: Invoke, map: MapDom[BasicBlock,D]) : MapDom[BasicBlock,D] = {
    val lastStmtOpt = block.data.getStmts.lastOption
    if(lastStmtOpt.nonEmpty && lastStmtOpt.get.isInstanceOf[Invoke]) {
      val invokeStmt = lastStmtOpt.get
      ???
    } else map
  }
  def evalReturn(cfg: CFG, block: BasicBlock, stmt: Return, map: MapDom[BasicBlock,D]) : MapDom[BasicBlock,D] = {
    ???
  }

}
