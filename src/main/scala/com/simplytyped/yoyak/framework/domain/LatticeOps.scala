package com.simplytyped.yoyak.framework.domain

trait LatticeOps[D] {
  def <=(lhs: D, rhs: D) : Boolean
  def \/(lhs: D, rhs: D) : D
  val bottom : D
}
