package com.simplytyped.yoyak.framework.domain

trait ParOrdOps[D <: Galois] {
  def <=(lhs: D#Abst, rhs: D#Abst) : Option[Boolean]
}