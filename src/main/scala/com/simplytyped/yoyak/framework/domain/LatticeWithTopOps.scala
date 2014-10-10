package com.simplytyped.yoyak.framework.domain

trait LatticeWithTopOps[D] extends LatticeOps[D] {
  def isTop(v: D) : Boolean
}