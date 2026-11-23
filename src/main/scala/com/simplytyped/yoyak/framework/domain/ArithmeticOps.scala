package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.il.CommonIL.Value.Constant

trait ArithmeticOps[D] extends LatticeWithTopOps[D] {
  type Concrete

  def +(lhs: D, rhs: D): D
  def -(lhs: D, rhs: D): D
  def *(lhs: D, rhs: D): D
  def /(lhs: D, rhs: D): D

  def lift(const: Constant): D
  def unlift(abs: D): Option[Set[Concrete]]
}
