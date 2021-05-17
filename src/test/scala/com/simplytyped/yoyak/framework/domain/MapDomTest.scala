package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.Galois.SetAbstraction
import com.simplytyped.yoyak.framework.domain.MapDomTest.SetInt
import org.scalatest.{FunSuite, Matchers}

class MapDomTest extends FunSuite with Matchers {
  test("MapDom ordering (<=) test: unrelated") {
    val map1     = MapDom.empty[Int, SetInt].update(1 -> Set(1, 2, 3))
    val map2     = MapDom.empty[Int, SetInt].update(2 -> Set(1, 2, 3))
    val ordering = MapDom.ops[Int, SetInt]

    ordering.partialCompare(map1, map2).isNaN should be(true)
  }
  test("MapDom ordering (<=) test: lhs is bigger") {
    val map1     = MapDom.empty[Int, SetInt].update(1 -> Set(1, 2, 3, 4))
    val map2     = MapDom.empty[Int, SetInt].update(1 -> Set(1, 2, 3))
    val ordering = MapDom.ops[Int, SetInt]

    ordering.partialCompare(map1, map2) should be(1.0)
  }
  test("MapDom ordering (<=) test: lhs is bigger 2") {
    val map1 = MapDom
      .empty[Int, SetInt]
      .update(1 -> Set(1, 2, 3))
      .update(2 -> Set(1, 3, 4))
    val map2 =
      MapDom.empty[Int, SetInt].update(1 -> Set(1, 2, 3)).update(2 -> Set(1))
    val ordering = MapDom.ops[Int, SetInt]

    ordering.partialCompare(map1, map2) should be(1.0)
  }
  test("MapDom ordering (<=) test: rhs is bigger") {
    val map1     = MapDom.empty[Int, SetInt].update(1 -> Set(1, 2, 3))
    val map2     = MapDom.empty[Int, SetInt].update(1 -> Set(1, 2, 3, 4))
    val ordering = MapDom.ops[Int, SetInt]

    ordering.partialCompare(map1, map2) should be(-1.0)
  }
  test("MapDom ordering (<=) test: lhs has more elements") {
    val map1 =
      MapDom.empty[Int, SetInt].update(1 -> Set(1, 2)).update(2 -> Set(2, 3, 4))
    val map2     = MapDom.empty[Int, SetInt].update(1 -> Set(1, 2, 3))
    val ordering = MapDom.ops[Int, SetInt]

    ordering.partialCompare(map1, map2).isNaN should be(true)
  }
  test("MapDom ordering (<=) test: rhs has more elements") {
    val map1 = MapDom.empty[Int, SetInt].update(1 -> Set(1, 2, 3))
    val map2 = MapDom
      .empty[Int, SetInt]
      .update(1 -> Set(1, 2, 3))
      .update(2 -> Set(2, 3, 4))
    val ordering = MapDom.ops[Int, SetInt]

    ordering.partialCompare(map1, map2) should be(-1.0)
  }
  test(
    "MapDom ordering (<=) test: rhs has more elements, but lhs has a bigger element"
  ) {
    val map1 = MapDom.empty[Int, SetInt].update(1 -> Set(1, 2, 3, 4))
    val map2 = MapDom
      .empty[Int, SetInt]
      .update(1 -> Set(1, 2, 3))
      .update(2 -> Set(2, 3, 4))
    val ordering = MapDom.ops[Int, SetInt]

    ordering.partialCompare(map1, map2).isNaN should be(true)
  }
}

object MapDomTest {
  type SetInt = SetAbstraction[Int]
}
