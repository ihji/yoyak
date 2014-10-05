package com.simplytyped.yoyak.framework.domain

trait ArithmeticOps[D] extends ParOrdOps[D] {
  def +(lhs: D, rhs: D) : D
}
