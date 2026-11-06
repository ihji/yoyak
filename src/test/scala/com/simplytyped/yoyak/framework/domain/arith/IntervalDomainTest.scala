package com.simplytyped.yoyak.framework.domain.arith

import com.simplytyped.yoyak.framework.domain.arith.Interval._
import com.simplytyped.yoyak.framework.domain.arith.IntervalInt.{
  arithOps,
  widening
}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class IntervalDomainTest extends AnyFunSuite with Matchers {
  private def interval(lower: Int, upper: Int): Interval =
    Interv.in(IInt(lower), IInt(upper))

  private val samples: Seq[Interval] = Seq(
    IntervBottom,
    IntervTop,
    Interv.of(-2),
    Interv.of(0),
    Interv.of(3),
    interval(-2, 0),
    interval(0, 3),
    interval(-2, 3),
    Interv.in(IInfMinus, IInt(0)),
    Interv.in(IInt(0), IInf)
  )

  test("bottom and top bound every interval") {
    samples.foreach { value =>
      arithOps.lteqv(IntervBottom, value) should be(true)
      arithOps.lteqv(value, IntervTop) should be(true)
      arithOps.isBottom(value) should be(value == IntervBottom)
      arithOps.isTop(value) should be(value == IntervTop)
    }
    arithOps.partialCompare(IntervBottom, IntervBottom) should be(0.0)
    arithOps.partialCompare(IntervTop, IntervTop) should be(0.0)
  }

  test("finite interval order agrees with concrete set inclusion") {
    val bounds = -3 to 3
    val finite = for {
      lower <- bounds
      upper <- lower to 3
    } yield (interval(lower, upper), (lower to upper).toSet)

    for {
      (left, leftValues)   <- finite
      (right, rightValues) <- finite
    } {
      val order = arithOps.partialCompare(left, right)
      if (leftValues == rightValues) order should be(0.0)
      else if (leftValues.subsetOf(rightValues)) order should be(-1.0)
      else if (rightValues.subsetOf(leftValues)) order should be(1.0)
      else order.isNaN should be(true)
    }
  }

  test("one-sided unbounded intervals use containment order") {
    val negative = Interv.in(IInfMinus, IInt(0))
    val positive = Interv.in(IInt(0), IInf)

    arithOps.partialCompare(interval(-3, 0), negative) should be(-1.0)
    arithOps.partialCompare(negative, interval(-3, 0)) should be(1.0)
    arithOps.partialCompare(interval(0, 3), positive) should be(-1.0)
    arithOps.partialCompare(positive, interval(0, 3)) should be(1.0)
    arithOps.partialCompare(negative, positive).isNaN should be(true)
    arithOps.partialCompare(positive, negative).isNaN should be(true)
  }

  test("join has bottom as identity and top as absorbing element") {
    samples.foreach { value =>
      arithOps.\/(value, IntervBottom) should be(value)
      arithOps.\/(IntervBottom, value) should be(value)
      arithOps.\/(value, IntervTop) should be(IntervTop)
      arithOps.\/(IntervTop, value) should be(IntervTop)
    }
  }

  test("join forms the convex hull of finite and infinite bounds") {
    arithOps.\/(interval(-3, -1), interval(2, 4)) should be(interval(-3, 4))
    arithOps.\/(interval(-3, 4), interval(-1, 2)) should be(interval(-3, 4))
    arithOps.\/(Interv.in(IInfMinus, IInt(-1)), interval(2, 4)) should be(
      Interv.in(IInfMinus, IInt(4))
    )
    arithOps.\/(interval(-3, -1), Interv.in(IInt(2), IInf)) should be(
      Interv.in(IInt(-3), IInf)
    )
    arithOps.\/(
      Interv.in(IInfMinus, IInt(-1)),
      Interv.in(IInt(2), IInf)
    ) should be(
      IntervTop
    )
  }

  test("join is idempotent commutative and associative") {
    samples.foreach(value => arithOps.\/(value, value) should be(value))
    for (left <- samples; right <- samples) {
      arithOps.\/(left, right) should be(arithOps.\/(right, left))
      samples.foreach { third =>
        arithOps.\/(arithOps.\/(left, right), third) should be(
          arithOps.\/(left, arithOps.\/(right, third))
        )
      }
    }
  }

  test("join is the least upper bound") {
    for (left <- samples; right <- samples) {
      val joined = arithOps.\/(left, right)
      arithOps.lteqv(left, joined) should be(true)
      arithOps.lteqv(right, joined) should be(true)
      samples.foreach { upper =>
        if (arithOps.lteqv(left, upper) && arithOps.lteqv(right, upper)) {
          arithOps.lteqv(joined, upper) should be(true)
        }
      }
    }
  }

  test("widening preserves stable or inward-moving finite bounds") {
    val previous = interval(-3, 4)
    widening.<>(previous, previous) should be(previous)
    widening.<>(previous, interval(-1, 2)) should be(previous)
    widening.<>(previous, interval(-3, 2)) should be(previous)
    widening.<>(previous, interval(-1, 4)) should be(previous)
  }

  test("widening sends only outward-moving bounds to infinity") {
    val previous = interval(-3, 4)
    widening.<>(previous, interval(-4, 4)) should be(
      Interv.in(IInfMinus, IInt(4))
    )
    widening.<>(previous, interval(-3, 5)) should be(
      Interv.in(IInt(-3), IInf)
    )
    widening.<>(previous, interval(-4, 5)) should be(IntervTop)
  }

  test("widening initializes from bottom and stabilizes ascending chains") {
    samples.foreach { value =>
      widening.<>(IntervBottom, value) should be(value)
      widening.<>(IntervTop, value) should be(IntervTop)
      widening.<>(value, IntervTop) should be(IntervTop)
    }
    val initial = interval(0, 1)
    val widened = widening.<>(initial, interval(0, 2))
    widened should be(Interv.in(IInt(0), IInf))
    widening.<>(widened, interval(0, 100)) should be(widened)
    arithOps.lteqv(initial, widened) should be(true)
    arithOps.lteqv(interval(0, 2), widened) should be(true)
  }

  test("concretization enumerates finite intervals and bottom exactly") {
    arithOps.unlift(IntervBottom) should be(Some(Set.empty[Int]))
    arithOps.unlift(Interv.of(-2)) should be(Some(Set(-2)))
    arithOps.unlift(interval(-2, 2)) should be(Some(Set(-2, -1, 0, 1, 2)))
    arithOps.unlift(interval(2, 4)) should be(Some(Set(2, 3, 4)))
  }

  test("concretization does not enumerate unbounded intervals") {
    arithOps.unlift(IntervTop) should be(None)
    arithOps.unlift(Interv.in(IInfMinus, IInt(0))) should be(None)
    arithOps.unlift(Interv.in(IInt(0), IInf)) should be(None)
  }
}
