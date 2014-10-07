package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.il.CommonIL.Value.Constant

trait ArithmeticOps[D] extends ParOrdOps[D] {
  def +(lhs: D, rhs: D) : D
  def -(lhs: D, rhs: D) : D
  def *(lhs: D, rhs: D) : D
  def /(lhs: D, rhs: D) : D

  def lift(const: Constant) : D
  def unlift[T : Numeric](abs: D) : Option[Set[T]]
}

object ArithmeticOps {
  implicit val intArithmeticOps = new ArithmeticOps[Int] {
    val poOps = implicitly[ParOrdOps[Int]]
    def <=(lhs: Int, rhs: Int): Option[Boolean] = poOps.<=(lhs,rhs)

    def +(lhs: Int, rhs: Int): Int = lhs + rhs
    def -(lhs: Int, rhs: Int): Int = lhs - rhs
    def *(lhs: Int, rhs: Int): Int = lhs * rhs
    def /(lhs: Int, rhs: Int): Int = lhs / rhs

    def lift(const: Constant) : Int = {
      ???
    }
    def unlift[T: Numeric](abs: Int): Option[Set[T]] = {
      ???
    }
  }
}
