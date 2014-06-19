package com.simplytyped.yoyak.domain

trait LatticeOps[E] {
  def <=(lhs: E, rhs: E) : Boolean
  def \/(lhs: E, rhs: E) : E
}
