package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.il.CommonIL.Value.Constant

trait ArithmeticOps[D <: Galois] extends LatticeWithTopOps[D] {
  def +(lhs: D#Abst, rhs: D#Abst) : D#Abst
  def -(lhs: D#Abst, rhs: D#Abst) : D#Abst
  def *(lhs: D#Abst, rhs: D#Abst) : D#Abst
  def /(lhs: D#Abst, rhs: D#Abst) : D#Abst

  def lift(const: Constant) : D#Abst
  def unlift(abs: D#Abst) : Option[Set[D#Conc]]
}