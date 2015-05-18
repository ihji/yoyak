package com.simplytyped.yoyak.framework.semantics

import com.simplytyped.yoyak.framework.domain.Galois

trait Narrowing[D<:Galois] {
  def ><(x : D#Abst, y : D#Abst) : D#Abst
}
