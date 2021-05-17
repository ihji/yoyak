package com.simplytyped.yoyak.framework.domain

import cats.PartialOrder

trait LatticeOps[D <: Galois] extends PartialOrder[D#Abst] {
  def \/(lhs: D#Abst, rhs: D#Abst): D#Abst
  def bottom: D#Abst

  def isBottom(v: D#Abst): Boolean = {
    val isBot = partialCompare(v, bottom)
    assert(!isBot.isNaN, "every element should have an order with bottom")
    isBot == 0
  }
}
