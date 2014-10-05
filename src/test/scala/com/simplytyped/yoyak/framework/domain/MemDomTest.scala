package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.MemDom.AbsBox
import com.simplytyped.yoyak.il.CommonIL.Value.Local
import org.scalatest.{FunSuite, Matchers}

class MemDomTest extends FunSuite with Matchers {
  import MemDomTest.{boxOps,arithOps}
  test("add and retrieve things") {
    val mem = MemDom.empty[Int,Set[String]]
    val mem2 = mem.update(Local("x")->AbsBox(Set("x")))
    mem2.get(Local("x")) should be (AbsBox(Set("x")))
  }
}

object MemDomTest {
  implicit val boxOps : ParOrdOps[Set[String]] = new ParOrdOps[Set[String]] {
    override def <=(lhs: Set[String], rhs: Set[String]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None
  }
  implicit val arithOps : ArithmeticOps[Int] = new ArithmeticOps[Int] {
    override def +(lhs: Int, rhs: Int): Int = lhs + rhs

    override def <=(lhs: Int, rhs: Int): Option[Boolean] = if(lhs == rhs) Some(true) else None
  }
}