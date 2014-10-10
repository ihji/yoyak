package com.simplytyped.yoyak.framework.domain

trait ParOrdOps[D] {
  def <=(lhs: D, rhs: D) : Option[Boolean]
}