package com.simplytyped.yoyak.framework.semantics

trait Widening[D] {
  def <>(x: D, y: D): D
}

object Widening {
  def NoWidening[A]: Widening[A] =
    new Widening[A] {
      override def <>(x: A, y: A): A = y
    }
}
