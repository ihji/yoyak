package com.simplytyped.yoyak.framework.domain

trait LatticeOps[D] extends ParOrdOps[D] {
  def \/(lhs: D, rhs: D) : D
  def bottom : D

  def isBottom(v: D) : Boolean = {
    val isBottomOpt = <=(v,bottom)
    assert(isBottomOpt.nonEmpty,"every element should have an order with bottom")
    isBottomOpt.get
  }
}
