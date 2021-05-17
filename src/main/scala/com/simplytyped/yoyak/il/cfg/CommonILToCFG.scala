package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.il.CommonIL.{MethodSig, Method}
import com.simplytyped.yoyak.il.CommonIL.Statement._

import scala.collection.mutable.ListBuffer

class CommonILToCFG {
  def makeBasicBlocks(
      methodSig: MethodSig,
      stmts: List[CoreStmt]
  ): List[BasicBlock] = {
    def sliceAtFirstBranch(
        stmts: List[CoreStmt]
    ): (List[CoreStmt], Option[Stmt], List[CoreStmt]) = {
      val idxOfFirstBranch = stmts.indexWhere {
        case _: If     => true
        case _: Goto   => true
        case _: Invoke => true
        case _: Return => true
        case _         => false
      }
      if (idxOfFirstBranch == -1) (stmts, None, List.empty[CoreStmt])
      else {
        val (firstStmts, secondStmts) = stmts.splitAt(idxOfFirstBranch + 1)
        val target = firstStmts.last match {
          case If(_, t)  => Some(t.getStmt)
          case Goto(t)   => Some(t.getStmt)
          case _: Invoke => None
          case _: Return => None
        }
        (firstStmts, target, secondStmts)
      }
    }

    val output    = new ListBuffer[List[CoreStmt]]
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
    val finalOutput = output.flatMap { slice =>
      var input = slice
      val buf   = new ListBuffer[List[CoreStmt]]
      while (input.nonEmpty) {
        val idx = input.indexWhere(targetSet.apply, 1)
        if (idx == -1 || idx == 0) {
          buf.append(input)
          input = List.empty
        } else {
          val (first, second) = input.splitAt(idx)
          buf.append(first)
          input = second
        }
      }
      buf.toList
    }
    finalOutput.map { BasicBlock.apply(methodSig) }.toList
  }
  def insertAssume(cfg: CFG): CFG = {
    cfg.nodes.foreach { node =>
      node.data.getStmts.last match {
        case If(cond, target) =>
          val nexts = cfg.getNexts(node)
          if (nexts.size == 2) {
            nexts.foreach { n =>
              if (n.data.getStmts.find { _ == target.getStmt }.nonEmpty) {
                val newStmts = Assume(cond) :: n.data.getStmts
                n.data.setStmts(newStmts)
              } else {
                val newStmts = Assume(cond.negate) :: n.data.getStmts
                n.data.setStmts(newStmts)
              }
            }
          }
        case _ => // do nothing
      }
    }
    cfg
  }
  def removeIfandGoto(cfg: CFG): CFG = {
    cfg.nodes.foreach { node =>
      node.data.getStmts.last match {
        case _: If =>
          val newStmts =
            if (node.data.getStmts.size > 1) node.data.getStmts.dropRight(1)
            else List(Nop())
          node.data.setStmts(newStmts)
        case _: Goto =>
          val newStmts =
            if (node.data.getStmts.size > 1) node.data.getStmts.dropRight(1)
            else List(Nop())
          node.data.setStmts(newStmts)
        case _ => // do nothing
      }
    }
    cfg
  }
  def transform(method: Method): CFG = {
    def findByFirstStmt(
        blocks: List[BasicBlock],
        stmt: Stmt
    ): Option[BasicBlock] = blocks.find { _.data.getStmts.head == stmt }
    val entryBlock        = BasicBlock.getEntryBlock()
    val basicBlocks =
      entryBlock :: makeBasicBlocks(method.name, method.statements)
    val exitBlock = BasicBlock.getExitBlock()
    val edges = basicBlocks.sliding(2).foldLeft(List.empty[BasicEdge]) {
      (edgeList, blockOfTwo) =>
        blockOfTwo.head.data.getStmts.last match {
          case If(_, target) =>
            val targetBlock = findByFirstStmt(basicBlocks, target.getStmt).get
            if (blockOfTwo.size == 1)
              BasicEdge(
                blockOfTwo.head,
                targetBlock
              ) :: edgeList // XXX: this case shouldn't happen
            else
              BasicEdge(blockOfTwo.head, blockOfTwo.last) :: BasicEdge(
                blockOfTwo.head,
                targetBlock
              ) :: edgeList
          case Goto(target) =>
            val targetBlock = findByFirstStmt(basicBlocks, target.getStmt).get
            BasicEdge(blockOfTwo.head, targetBlock) :: edgeList
          case Return(_) => BasicEdge(blockOfTwo.head, exitBlock) :: edgeList
          case Throw(_)  => BasicEdge(blockOfTwo.head, exitBlock) :: edgeList
          case _ =>
            if (blockOfTwo.size == 1) edgeList
            else BasicEdge(blockOfTwo.head, blockOfTwo.last) :: edgeList
        }
    }
    val newCfg = CFG.empty
    val rawCfg = edges.foldLeft(newCfg) { _.addEdge(_) }
    (insertAssume _ andThen removeIfandGoto) apply rawCfg
  }
}
