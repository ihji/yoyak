package com.simplytyped.yoyak.framework.domain

trait LatticeWithTopOps[D <: Galois] extends LatticeOps[D] {
  def isTop(v: D#Abst) : Boolean
}