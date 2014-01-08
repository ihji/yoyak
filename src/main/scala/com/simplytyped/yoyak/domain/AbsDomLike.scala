package com.simplytyped.yoyak.domain

trait AbsDomLike[D <: AbsDomLike[D]] {
  def isTop : Boolean
  def <<=(other: D) : Option[Boolean]
  def ++(other: D) : D
}
