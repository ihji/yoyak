package com.simplytyped.yoyak.framework.semantics

import com.simplytyped.yoyak.framework.domain.Galois

trait Widening[D<:Galois] {
  def <>(x : D#Abst, y : D#Abst) : D#Abst
}

object Widening {
  def NoWidening[A<:Galois] : Widening[A] = new Widening[A] {
    override def <>(x: A#Abst, y: A#Abst): A#Abst = y
  }
}