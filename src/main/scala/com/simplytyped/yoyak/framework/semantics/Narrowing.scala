package com.simplytyped.yoyak.framework.semantics

trait Narrowing[D] {
  def ><(x: D, y: D): D
}
