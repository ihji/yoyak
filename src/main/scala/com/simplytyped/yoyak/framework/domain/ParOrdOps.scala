package com.simplytyped.yoyak.framework.domain

trait ParOrdOps[D] {
  def <=(lhs: D, rhs: D) : Option[Boolean]
}

object ParOrdOps {
  implicit val intParOrdOps = new ParOrdOps[Int] {
    override def <=(lhs: Int, rhs: Int): Option[Boolean] = if(lhs == rhs) Some(true) else None
  }
}