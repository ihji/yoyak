package com.simplytyped.yoyak.framework.domain

trait LatticeOps[D <: Galois] extends ParOrdOps[D] {
  def \/(lhs: D#Abst, rhs: D#Abst) : D#Abst
  def bottom : D#Abst

  def isBottom(v: D#Abst) : Boolean = {
    val isBottomOpt = <=(v,bottom)
    assert(isBottomOpt.nonEmpty,"every element should have an order with bottom")
    isBottomOpt.get
  }
}
