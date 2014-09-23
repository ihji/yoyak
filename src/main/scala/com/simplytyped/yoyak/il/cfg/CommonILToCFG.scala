package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.il.CommonIL.Statement._

import scala.collection.mutable.ListBuffer

class CommonILToCFG {
  def makeBasicBlocks(stmts: List[CoreStmt]) : List[BasicBlock] = {
    def sliceAtFirstBranch(stmts: List[CoreStmt]) : (List[CoreStmt],Option[Stmt],List[CoreStmt]) = {
      val idxOfFirstBranch = stmts.indexWhere{
        case _ : If => true
        case _ : Goto => true
        case _ => false
      }
      if(idxOfFirstBranch == -1) (stmts,None,List.empty[CoreStmt])
      else {
        val (firstStmts, secondStmts) = stmts.splitAt(idxOfFirstBranch+1)
        val target = firstStmts.last match {
          case If(_,t) => t
          case Goto(t) => t
        }
        (firstStmts,Some(target.getStmt),secondStmts)
      }
    }

    val output = new ListBuffer[List[CoreStmt]]
    var targetSet = Set.empty[Stmt]

    // split branch sources
    {
      var input = stmts
      while (input.nonEmpty) {
        val (slice, targetOpt, remaining) = sliceAtFirstBranch(input)
        output.append(slice)
        if (targetOpt.nonEmpty) targetSet += targetOpt.get
        input = remaining
      }
    }

    // split branch targets
    val finalOutput = output.flatMap{
      slice =>
        var input = slice
        val buf = new ListBuffer[List[CoreStmt]]
        while(input.nonEmpty) {
          val idx = input.indexWhere(targetSet.apply,1)
          if(idx == -1 || idx == 0) {
            buf.append(input)
            input = List.empty
          } else {
            val (first,second) = input.splitAt(idx)
            buf.append(first)
            input = second
          }
        }
        buf.toList
    }
    finalOutput.map{BasicBlock.apply}.toList
  }
  def insertAssume(cfg: CFG) : CFG = {
    cfg.nodes.foreach{ node =>
      node.data.getStmts.last match {
        case If(cond,target) =>
          val nexts = cfg.getNexts(node)
          assert(nexts.size == 2)
          nexts.foreach { n =>
            if(n.data.getStmts.find{_ ==target.getStmt}.nonEmpty) {
              val newStmts = Assume(cond)::n.data.getStmts
              n.data.setStmts(newStmts)
            } else {
              val newStmts = Assume(cond.negate)::n.data.getStmts
              n.data.setStmts(newStmts)
            }
          }
        case _ => // do nothing
      }
    }
    cfg
  }
  def transform(stmts: List[CoreStmt]) : CFG = {
    def findByFirstStmt(blocks: List[BasicBlock], stmt: Stmt) : Option[BasicBlock] = blocks.find{_.data.getStmts.head == stmt}
    val basicBlocks = makeBasicBlocks(stmts)
    val edges = basicBlocks.sliding(2).foldLeft(List.empty[BasicEdge]) {
      (edgeList,blockOfTwo) =>
        blockOfTwo.head.data.getStmts.last match {
          case If(_,target) =>
            val targetBlock = findByFirstStmt(basicBlocks,target.getStmt).get
            if(blockOfTwo.size == 1) BasicEdge(blockOfTwo.head,targetBlock)::edgeList // XXX: this case shouldn't happen
            else BasicEdge(blockOfTwo.head,blockOfTwo.last)::BasicEdge(blockOfTwo.head,targetBlock)::edgeList
          case Goto(target) =>
            val targetBlock = findByFirstStmt(basicBlocks,target.getStmt).get
            BasicEdge(blockOfTwo.head,targetBlock)::edgeList
          case Return(_) => edgeList
          case Throw(_) => edgeList
          case _ =>
            if(blockOfTwo.size == 1) edgeList
            else BasicEdge(blockOfTwo.head,blockOfTwo.last)::edgeList
        }
    }
    val newCfg = CFG.empty
    val rawCfg = edges.foldLeft(newCfg) {_.addEdge(_)}
    insertAssume(rawCfg)
  }
}
