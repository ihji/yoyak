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
  class SetAbstraction[A] extends Galois {
    type Conc = A
    type Abst = Set[A]
  }
  object SetAbstraction {
    implicit def boxedOps[A]: LatticeWithTopOps[SetAbstraction[A]] = new LatticeWithTopOps[SetAbstraction[A]] {
      override def isTop(v: Set[A]): Boolean = false

      override def bottom: Set[A] = Set.empty[A]

      override def \/(lhs: Set[A], rhs: Set[A]): Set[A] = lhs ++ rhs

      override def partialCompare(lhs: Set[A], rhs: Set[A]): Double =
        if(lhs == rhs) 0.0
        else if(lhs subsetOf rhs) -1.0
        else if(rhs subsetOf lhs) 1.0
        else Double.NaN
    }
  }
}