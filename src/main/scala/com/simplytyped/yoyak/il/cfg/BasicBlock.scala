package com.simplytyped.yoyak.il.cfg

import com.simplytyped.yoyak.graph.NodeLike
import com.simplytyped.yoyak.il.CommonIL.Statement.CoreStmt

case class BasicBlock(data: List[CoreStmt]) extends NodeLike[BasicBlock] {
  type D = List[CoreStmt]
}
