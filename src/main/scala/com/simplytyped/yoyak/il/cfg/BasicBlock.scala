package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.NodeLike
import com.simplytyped.yoyak.il.CommonIL.Statement.Stmt

case class BasicBlock(data: List[Stmt]) extends NodeLike[BasicBlock] {
  type D = List[Stmt]
}
