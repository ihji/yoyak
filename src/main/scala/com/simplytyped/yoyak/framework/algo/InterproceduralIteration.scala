package com.simplytyped.yoyak.framework.algo

import com.simplytyped.yoyak.framework.domain.MapDom
import com.simplytyped.yoyak.framework.domain.mem.{MemDomLike, Resolvable, Localizable}
import com.simplytyped.yoyak.il.CommonIL.Program
import com.simplytyped.yoyak.il.CommonIL.Statement.{Return, Invoke}
import com.simplytyped.yoyak.il.CommonIL.Value.{Local, Param}
import com.simplytyped.yoyak.il.cfg.BasicEdge.{IntraEdge, EdgeType, InterEdge}
import com.simplytyped.yoyak.il.cfg.{CFG, BasicEdge, BasicBlock}

trait InterproceduralIteration[A,D,M<:MemDomLike[A,D,M]] extends FlowSensitiveIteration[M] with CfgNavigator[M] {
  implicit val localize : Localizable[M]
  implicit val resolve  : Resolvable[M]

  val pgm : Program
  val worklist : Worklist[BasicBlock]

  var interproceduralEdges : CFG = CFG.empty

  override def getNextBlocks(bb: BasicBlock) : Seq[BasicBlock] = {
    val methodSigOpt = BasicBlock.getMethodSig(bb)
    methodSigOpt.flatMap {pgm.methods.get}.
      flatMap {_.cfg}.
      map {_.getNexts(bb).toList}.
      getOrElse(List.empty[BasicBlock])
  }
  override def memoryFetcher(map: MapDom[BasicBlock,M], b: BasicBlock) : Seq[M] = {
    val methodSigOpt = BasicBlock.getMethodSig(b)
    methodSigOpt.flatMap {pgm.methods.get}.
      flatMap {_.cfg}.
      flatMap {_.prevs.get(b)}.
      map {_.toList.map{e => renameReturnVar(map.get(e.from),e.label)}}.
      getOrElse(List.empty[M])
  }

  private def renameReturnVar(input: M, edgeTy: EdgeType) : M = {
    edgeTy match {
      case IntraEdge => input
      case InterEdge(retVar) =>
        // ideally, we should remove all objects where they're unable to be accessed without the return variable
        if(retVar.isEmpty) input.remove(localize.returningPlaceholder)
        else {
          val data = input.get(localize.returningPlaceholder)
          input.update(retVar.get->data)
        }
    }
  }

  override def work(map: MapDom[BasicBlock,M], input: M, block: BasicBlock) : (MapDom[BasicBlock,M],M) = {
    val stmts = block.data.getStmts
    val (newMap,output) = stmts.foldLeft(map,input){
      case ((m,in),ivk @ Invoke(_,_)) =>
        (invoking(block, ivk, in, m), absTransfer.transfer(in, ivk))
      case ((m,in),ret @ Return(_)) =>
        (returning(block, ret, in, m), absTransfer.transfer(in, ret))
      case ((m,in),s) => (m, absTransfer.transfer(in, s))
    }
    (newMap,output)
  }

  /*
  invoking procedure:
  1. map arguments to parameters
  2. remove locals
  3. resolve callees and draw inter-procedural edges
  4. put memory at callees' entries (join?)
  5. push successors of callees' entries into a worklist
  6. push intra-procedural successors of this invoke block into a worklist
     (maybe no need to be handled here)

  returning procedure:
  1. remove locals
  2. get a returning value and save it into a returning memory at the special location
   something like ($__ret)
  3. put the returning memory at the exit node (join?)
  4. push successors of this exit node into a worklist

  fetching input memory:
  1. get predecessors and their edge types
  2. if the edge type is inter-procedural,
  fetch the memory and
  rename the special location to the actual return variable as shown in the edge
  3. join all input memories
   */

  def mapArguments(invokeStmt: Invoke, input: M) : M = {
    val argsWithIndex = invokeStmt.callee.args.map{input.get}.zipWithIndex
    val newInput = argsWithIndex.foldLeft(input) {
      case (m,(v,i)) => m.update(Param(i) -> v)
    }
    newInput
  }

  def invoking(block: BasicBlock, invokeStmt: Invoke, input: M, map: MapDom[BasicBlock,M]) : MapDom[BasicBlock,M] = {
    val targetMethodSigs = resolve.resolve(input,invokeStmt)
    val targetMethods = targetMethodSigs.flatMap{pgm.methods.get}
    targetMethods.foldLeft(map) {
      case (m,targetMethod) =>
        val argMappedMemory = mapArguments(invokeStmt,input)
        val localizedMemory = localize.deleteLocals(argMappedMemory)
        var newM = m
        for (targetCfg <- targetMethod.cfg;
             targetEntry <- targetCfg.getEntry; targetExit <- targetCfg.getExit) {

          val nextBlocks = getNextBlocks(block)
          assert(nextBlocks.size == 1, "invoke block can only have one next block")
          val nextBlock = nextBlocks.head
          val goEdge = BasicEdge(block, targetEntry, InterEdge(None))
          val returnEdge = BasicEdge(targetExit, nextBlock, InterEdge(invokeStmt.ret))
          interproceduralEdges = interproceduralEdges.addEdge(goEdge).addEdge(returnEdge)

          newM = m.weakUpdate(targetEntry,localizedMemory)

          val succsOfTargetEntry = targetCfg.getNexts(targetEntry).toList
          worklist.add(succsOfTargetEntry:_*)
        }
        newM
    }
  }

  def returning(block: BasicBlock, returnStmt: Return, input: M, map: MapDom[BasicBlock,M]) : MapDom[BasicBlock,M] = {
    val returnValOpt = returnStmt.v.map{input.get}
    val localizedMemory = localize.deleteLocals(input)
    val returningMemory = returnValOpt.foldLeft(localizedMemory) {
      case (m,ret) => m.update(localize.returningPlaceholder,ret)
    }
    val exitNode = getNextBlocks(block)
    exitNode.foldLeft(map) {
      case (m,exitNode) =>
        val succsOfExit = interproceduralEdges.getNexts(exitNode).toList
        worklist.add(succsOfExit:_*)
        m.weakUpdate(exitNode->returningMemory)
    }
  }
}
