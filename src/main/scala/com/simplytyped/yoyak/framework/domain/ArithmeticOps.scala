package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.il.CommonIL.Value.Constant

trait ArithmeticOps[D] extends ParOrdOps[D] {
  def +(lhs: D, rhs: D) : D
  def -(lhs: D, rhs: D) : D
  def *(lhs: D, rhs: D) : D
  def /(lhs: D, rhs: D) : D
}

object ArithmeticOps {
  implicit val intArithmeticOps = new ArithmeticOps[Int] {
    val poOps = implicitly[ParOrdOps[Int]]
    override def <=(lhs: Int, rhs: Int): Option[Boolean] = poOps.<=(lhs,rhs)

    override def +(lhs: Int, rhs: Int): Int = lhs + rhs
    override def -(lhs: Int, rhs: Int): Int = lhs - rhs
    override def *(lhs: Int, rhs: Int): Int = lhs * rhs
    override def /(lhs: Int, rhs: Int): Int = lhs / rhs
  }
}
