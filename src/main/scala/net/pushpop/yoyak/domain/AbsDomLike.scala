package net.pushpop.yoyak.domain

trait AbsDomLike[D <: AbsDomLike[D]] {
  def isTop : Boolean
  def <<=(other: D) : Option[Boolean]
  def ++(other: D) : D
}
