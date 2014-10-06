package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.MemDom._
import com.simplytyped.yoyak.il.CommonIL.Value._
import org.scalatest.{FunSuite, Matchers}

class MemDomTest extends FunSuite with Matchers {
  import MemDomTest.boxOps
  test("add and retrieve things") {
    val mem = MemDom.empty[Int,Set[String]]
    val mem2 = mem.update(Local("x")->AbsBox(Set("x")))
    mem2.get(Local("x")) should be (AbsBox(Set("x")))
  }
  test("add and retrieve things in field") {
    val mem = MemDom.empty[Int,Set[String]]
    val mem2 = mem.alloc(Local("x"))
    val mem3 = mem2.update(InstanceFieldRef(Local("x"),"f")->AbsArith(10))
    mem3.get(InstanceFieldRef(Local("x"),"f")) should be (AbsArith(10))
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