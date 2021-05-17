package com.simplytyped.yoyak.framework.algo

class Worklist[E] {
  var list = Vector.empty[E]
  def add(elems: E*): Worklist[E] = {
    list = list ++ elems
    this
  }
  def pop(): Option[E] = {
    val elem = list.headOption
    if (elem.nonEmpty) list = list.drop(1)
    elem
  }
  def size() = list.size
}

object Worklist {
  def empty[E] = new Worklist[E]
}
