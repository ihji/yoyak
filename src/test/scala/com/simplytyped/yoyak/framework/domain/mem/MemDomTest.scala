package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.{ArithmeticOps, LatticeWithTopOps}
import com.simplytyped.yoyak.framework.domain.mem.MemElems._
import com.simplytyped.yoyak.il.CommonIL.ClassName
import com.simplytyped.yoyak.il.CommonIL.Value._
import org.scalatest.{FunSuite, Matchers}

class MemDomTest extends FunSuite with Matchers {
  import com.simplytyped.yoyak.framework.domain.mem.MemDomTest.{arithOps,boxOps}
  test("add and retrieve things") {
    val mem = MemDom.empty[Set[Int],Set[String]]
    val mem2 = mem.update(Local("x")->AbsBox(Set("x")))
    mem2.get(Local("x")) should be (AbsBox(Set("x")))
  }
  test("add and retrieve things in instance field") {
    val mem = MemDom.empty[Set[Int],Set[String]]
    val mem2 = mem.alloc(Local("x"))
    val mem3 = mem2.update(InstanceFieldRef(Local("x"),"f")->AbsArith(Set(10)))
    mem3.get(InstanceFieldRef(Local("x"),"f")) should be (AbsArith(Set(10)))
    mem3.get(InstanceFieldRef(Local("x"),"g")) should be (AbsBottom)
  }
  test("add and retrieve things in static field") {
    val mem = MemDom.empty[Set[Int],Set[String]]
    val mem2 = mem.update(StaticFieldRef(ClassName("yoyak.Test"),"f")->AbsArith(Set(10)))
    mem2.get(StaticFieldRef(ClassName("yoyak.Test"),"f")) should be (AbsArith(Set(10)))
    mem2.get(StaticFieldRef(ClassName("yoyak.Test"),"g")) should be (AbsBottom)
  }
  test("add and retrieve things in array") {
    val mem = MemDom.empty[Set[Int],Set[String]]
    val mem2 = mem.alloc(Local("x"))
    val mem3 = mem2.update(ArrayRef(Local("x"),IntegerConstant(1))->AbsArith(Set(10)))
    mem3.get(ArrayRef(Local("x"),IntegerConstant(1))) should be (AbsArith(Set(10)))
    mem3.get(ArrayRef(Local("x"),IntegerConstant(2))) should be (AbsArith(Set(10)))
    // TODO: test weak update. x[0] = 10; x[1] = 20; get x[0]
  }
}

object MemDomTest {
  implicit val boxOps : LatticeWithTopOps[Set[String]] = new LatticeWithTopOps[Set[String]] {
    override def <=(lhs: Set[String], rhs: Set[String]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None

    override def \/(lhs: Set[String], rhs: Set[String]): Set[String] = lhs ++ rhs

    override def bottom: Set[String] = Set.empty[String]

    override def isTop(v: Set[String]): Boolean = ???
  }
  implicit val arithOps : ArithmeticOps[Set[Int]] = new ArithmeticOps[Set[Int]] {
    override def +(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def /(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def -(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def *(lhs: Set[Int], rhs: Set[Int]): Set[Int] = ???

    override def lift(const: Constant): Set[Int] = ???

    override def unlift[T: Numeric](abs: Set[Int]): Option[Set[T]] = ???

    override def isTop(v: Set[Int]): Boolean = ???

    override def bottom: Set[Int] = Set.empty[Int]

    override def \/(lhs: Set[Int], rhs: Set[Int]): Set[Int] = lhs ++ rhs

    override def <=(lhs: Set[Int], rhs: Set[Int]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None
  }
}