package com.simplytyped.yoyak.framework.domain

import cats.PartialOrder

trait LatticeOps[D] extends PartialOrder[D] {
  def \/(lhs: D, rhs: D): D
  def bottom: D

  def isBottom(v: D): Boolean = {
    val isBot = partialCompare(v, bottom)
    assert(!isBot.isNaN, "every element should have an order with bottom")
    isBot == 0
  }
}
