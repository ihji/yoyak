package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.Galois.SetAbstraction
import com.simplytyped.yoyak.framework.domain.MapDomTest.SetInt
import com.simplytyped.yoyak.framework.domain.mem.MemDomTest.SetString
import com.simplytyped.yoyak.framework.domain.{
  ArithmeticOps,
  Galois,
  LatticeWithTopOps
}
import com.simplytyped.yoyak.framework.domain.mem.MemElems._
import com.simplytyped.yoyak.il.CommonIL.ClassName
import com.simplytyped.yoyak.il.CommonIL.Statement.Nop
import com.simplytyped.yoyak.il.CommonIL.Value._
import org.scalatest.{FunSuite, Matchers}

class MemDomTest extends FunSuite with Matchers {
  import com.simplytyped.yoyak.framework.domain.mem.MemDomTest.arithOps
  test("add and retrieve things") {
    val mem  = MemDom.empty[SetInt, SetString]
    val mem2 = mem.update(Local("x") -> AbsBox[SetString](Set("x")))
    mem2.get(Local("x")) should be(AbsBox[SetString](Set("x")))
  }
  test("add and retrieve things in instance field") {
    val mem            = MemDom.empty[SetInt, SetString]
    val (newRef, mem2) = mem.alloc(Nop())
    val mem3           = mem2.update(Local("x") -> newRef)
    val mem4 = mem3.update(
      InstanceFieldRef(Local("x"), "f") -> AbsArith[SetInt](Set(10))
    )
    mem4.get(InstanceFieldRef(Local("x"), "f")) should be(
      AbsArith[SetInt](Set(10))
    )
    mem4.get(InstanceFieldRef(Local("x"), "g")) should be(AbsBottom)
  }
  test("add and retrieve things in static field") {
    val mem = MemDom.empty[SetInt, SetString]
    val mem2 = mem.update(
      StaticFieldRef(ClassName("yoyak.Test"), "f") -> AbsArith[SetInt](Set(10))
    )
    mem2.get(StaticFieldRef(ClassName("yoyak.Test"), "f")) should be(
      AbsArith[SetInt](Set(10))
    )
    mem2.get(StaticFieldRef(ClassName("yoyak.Test"), "g")) should be(AbsBottom)
  }
  test("add and retrieve things in array") {
    val mem            = MemDom.empty[SetInt, SetString]
    val (newref, mem2) = mem.alloc(Nop())
    val mem3           = mem2.update(Local("x") -> newref)
    val mem4 = mem3.update(
      ArrayRef(Local("x"), IntegerConstant(1)) -> AbsArith[SetInt](Set(10))
    )
    mem4.get(ArrayRef(Local("x"), IntegerConstant(1))) should be(
      AbsArith[SetInt](Set(10))
    )
    mem4.get(ArrayRef(Local("x"), IntegerConstant(2))) should be(
      AbsArith[SetInt](Set(10))
    )
  }
  test("add and retrieve things in array with join") {
    val mem            = MemDom.empty[SetInt, SetString]
    val (newref, mem2) = mem.alloc(Nop())
    val mem3           = mem2.update(Local("x") -> newref)
    val mem4 = mem3.update(
      ArrayRef(Local("x"), IntegerConstant(1)) -> AbsArith[SetInt](Set(10))
    )
    val mem5 = mem4.update(
      ArrayRef(Local("x"), IntegerConstant(2)) -> AbsArith[SetInt](Set(20))
    )
    mem5.get(ArrayRef(Local("x"), IntegerConstant(1))) should be(
      AbsArith[SetInt](Set(10, 20))
    )
    mem5.get(ArrayRef(Local("x"), IntegerConstant(2))) should be(
      AbsArith[SetInt](Set(10, 20))
    )
  }
}

object MemDomTest {
  type SetString = SetAbstraction[String]

  implicit val arithOps: ArithmeticOps[SetInt] = new ArithmeticOps[SetInt] {
    override def +(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def /(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def -(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def *(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def lift(const: Constant): Set[Int] = ???

    override def unlift(abs: Set[Int]): Option[Set[Int]] = ???

    override def isTop(v: Set[Int]): Boolean = false

    override def bottom: Set[Int] = Set.empty[Int]

    override def \/(lhs: Set[Int], rhs: Set[Int]): Set[Int] = lhs ++ rhs

    override def partialCompare(lhs: Set[Int], rhs: Set[Int]): Double =
      if (lhs == rhs) 0.0
      else if (lhs subsetOf rhs) -1.0
      else if (rhs subsetOf lhs) 1.0
      else Double.NaN
  }
}
