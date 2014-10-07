package com.simplytyped.yoyak.framework.domain.mem

import com.simplytyped.yoyak.framework.domain.ParOrdOps
import com.simplytyped.yoyak.framework.domain.mem.MemElems._
import com.simplytyped.yoyak.il.CommonIL.ClassName
import com.simplytyped.yoyak.il.CommonIL.Value._
import org.scalatest.{FunSuite, Matchers}

class MemDomTest extends FunSuite with Matchers {
  import com.simplytyped.yoyak.framework.domain.mem.MemDomTest.boxOps
  test("add and retrieve things") {
    val mem = MemDom.empty[Int,Set[String]]
    val mem2 = mem.update(Local("x")->AbsBox(Set("x")))
    mem2.get(Local("x")) should be (AbsBox(Set("x")))
  }
  test("add and retrieve things in instance field") {
    val mem = MemDom.empty[Int,Set[String]]
    val mem2 = mem.alloc(Local("x"))
    val mem3 = mem2.update(InstanceFieldRef(Local("x"),"f")->AbsArith(10))
    mem3.get(InstanceFieldRef(Local("x"),"f")) should be (AbsArith(10))
    mem3.get(InstanceFieldRef(Local("x"),"g")) should be (AbsBottom)
  }
  test("add and retrieve things in static field") {
    val mem = MemDom.empty[Int,Set[String]]
    val mem2 = mem.update(StaticFieldRef(ClassName("yoyak.Test"),"f")->AbsArith(10))
    mem2.get(StaticFieldRef(ClassName("yoyak.Test"),"f")) should be (AbsArith(10))
    mem2.get(StaticFieldRef(ClassName("yoyak.Test"),"g")) should be (AbsBottom)
  }
}

object MemDomTest {
  implicit val boxOps : ParOrdOps[Set[String]] = new ParOrdOps[Set[String]] {
    override def <=(lhs: Set[String], rhs: Set[String]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None
  }
}