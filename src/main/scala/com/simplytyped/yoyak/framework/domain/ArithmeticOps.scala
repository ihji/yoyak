package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.il.CommonIL.Value.Constant

trait ArithmeticOps[D] extends LatticeWithTopOps[D] {
  def +(lhs: D, rhs: D) : D
  def -(lhs: D, rhs: D) : D
  def *(lhs: D, rhs: D) : D
  def /(lhs: D, rhs: D) : D

  def lift(const: Constant) : D
  def unlift[T : Numeric](abs: D) : Option[Set[T]]
}
