package com.simplytyped.yoyak.framework.domain.arith

import com.simplytyped.yoyak.framework.domain.arith.Interval._
import com.simplytyped.yoyak.framework.domain.arith.IntervalInt.arithOps
import org.scalatest.{Matchers, FunSuite}

class IntervalTest extends FunSuite with Matchers {
  test("add operation 1") {
    val interv2 = Interv.of(2)
    val interv3 = Interv.of(3)
    val interv5 = Interv.of(5)
    arithOps.+(interv2,interv3) should be (interv5)
  }
  test("add operation 2") {
    val interv1 = Interv.in(IInt(-2),IInt(3))
    val interv2 = Interv.of(3)
    val interv3 = Interv.in(IInt(1),IInt(6))
    arithOps.+(interv1,interv2) should be (interv3)
  }
  test("add operation 3") {
    val interv1 = Interv.in(IInfMinus,IInt(3))
    val interv2 = Interv.of(100)
    val interv3 = Interv.in(IInfMinus,IInt(103))
    arithOps.+(interv1,interv2) should be (interv3)
  }
  test("add operation 4") {
    val interv1 = Interv.in(IInfMinus,IInf)
    val interv2 = Interv.of(100)
    val interv3 = IntervTop
    arithOps.+(interv1,interv2) should be (interv3)
  }
  test("add operation 5") {
    val interv1 = Interv.in(IInfMinus,IInt(32))
    val interv2 = Interv.in(IInt(100),IInf)
    val interv3 = IntervTop
    arithOps.+(interv1,interv2) should be (interv3)
  }
  test("sub operation 1") {
    val interv2 = Interv.of(2)
    val interv3 = Interv.of(3)
    val intervM1 = Interv.of(-1)
    arithOps.-(interv2,interv3) should be (intervM1)
  }
  test("sub operation 2") {
    val interv1 = Interv.in(IInt(-2),IInt(3))
    val interv2 = Interv.of(3)
    val interv3 = Interv.in(IInt(-5),IInt(0))
    arithOps.-(interv1,interv2) should be (interv3)
  }
  test("sub operation 3") {
    val interv1 = Interv.in(IInfMinus,IInt(3))
    val interv2 = Interv.of(100)
    val interv3 = Interv.in(IInfMinus,IInt(-97))
    arithOps.-(interv1,interv2) should be (interv3)
  }
  test("sub operation 4") {
    val interv1 = Interv.in(IInfMinus,IInf)
    val interv2 = Interv.of(100)
    val interv3 = IntervTop
    arithOps.-(interv1,interv2) should be (interv3)
  }
  test("sub operation 5") {
    val interv1 = Interv.in(IInfMinus,IInt(32))
    val interv2 = Interv.in(IInt(100),IInf)
    val interv3 = Interv.in(IInfMinus,IInt(-68))
    arithOps.-(interv1,interv2) should be (interv3)
  }
  test("mult operation 1") {
    val interv2 = Interv.of(2)
    val interv3 = Interv.of(3)
    val interv6 = Interv.of(6)
    arithOps.*(interv2,interv3) should be (interv6)
  }
  test("mult operation 2") {
    val interv1 = Interv.in(IInt(-2),IInt(3))
    val interv2 = Interv.of(3)
    val interv3 = Interv.in(IInt(-6),IInt(9))
    arithOps.*(interv1,interv2) should be (interv3)
  }
  test("mult operation 3") {
    val interv1 = Interv.in(IInfMinus,IInt(32))
    val interv2 = Interv.in(IInt(100),IInf)
    val interv3 = IntervTop
    arithOps.*(interv1,interv2) should be (interv3)
  }
  test("div operation 1") {
    val interv1 = Interv.of(15)
    val interv2 = Interv.of(3)
    val interv3 = Interv.of(5)
    arithOps./(interv1,interv2) should be (interv3)
  }
  test("div operation 2") {
    val interv1 = Interv.in(IInt(-99),IInt(3))
    val interv2 = Interv.of(3)
    val interv3 = Interv.in(IInt(-33),IInt(1))
    arithOps./(interv1,interv2) should be (interv3)
  }
  test("div operation 3") {
    val interv1 = Interv.in(IInfMinus,IInt(32))
    val interv2 = Interv.in(IInt(100),IInf)
    val interv3 = Interv.in(IInfMinus,IInt(0))
    arithOps./(interv1,interv2) should be (interv3)
  }
  test("div operation 4") {
    val interv1 = Interv.of(15)
    val interv2 = Interv.of(0)
    val interv3 = IntervBottom
    arithOps./(interv1,interv2) should be (interv3)
  }
  test("div operation 5") {
    val interv1 = Interv.in(IInfMinus,IInt(0))
    val interv2 = Interv.of(0)
    val interv3 = IntervBottom
    arithOps./(interv1,interv2) should be (interv3)
  }
}
