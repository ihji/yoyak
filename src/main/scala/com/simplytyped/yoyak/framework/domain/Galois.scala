package com.simplytyped.yoyak.framework.domain

trait Galois {
  type Conc
  type Abst
}

object Galois {
  class GaloisIdentity[A] extends Galois {
    type Conc = A
    type Abst = A
  }
}