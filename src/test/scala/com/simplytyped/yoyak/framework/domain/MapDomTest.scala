package com.simplytyped.yoyak.framework.domain

import org.scalatest.{Matchers, FunSuite}

class MapDomTest extends FunSuite with Matchers {
  test("MapDom ordering (<=) test: unrelated") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3))
    val map2 = MapDom.empty[Int,Set[Int]].update(2->Set(1,2,3))
    val ordering = MapDom.ops[Int,Set[Int]]

    ordering.<=(map1,map2) should be (None)
  }
  test("MapDom ordering (<=) test: lhs is bigger") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3,4))
    val map2 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3))
    val ordering = MapDom.ops[Int,Set[Int]]

    ordering.<=(map1,map2) should be (Some(false))
  }
  test("MapDom ordering (<=) test: rhs is bigger") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3))
    val map2 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3,4))
    val ordering = MapDom.ops[Int,Set[Int]]

    ordering.<=(map1,map2) should be (Some(true))
  }
  test("MapDom ordering (<=) test: lhs has more elements") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2)).update(2->Set(2,3,4))
    val map2 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3))
    val ordering = MapDom.ops[Int,Set[Int]]

    ordering.<=(map1,map2) should be (None)
  }
  test("MapDom ordering (<=) test: rhs has more elements") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3))
    val map2 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3)).update(2->Set(2,3,4))
    val ordering = MapDom.ops[Int,Set[Int]]

    ordering.<=(map1,map2) should be (Some(true))
  }
  test("MapDom ordering (<=) test: rhs has more elements, but lhs has a bigger element") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3,4))
    val map2 = MapDom.empty[Int,Set[Int]].update(1->Set(1,2,3)).update(2->Set(2,3,4))
    val ordering = MapDom.ops[Int,Set[Int]]

    ordering.<=(map1,map2) should be (None)
  }
}

object MapDomTest {
  implicit val ops : LatticeOps[Set[Int]] = new LatticeOps[Set[Int]] {
    override def <=(lhs: Set[Int], rhs: Set[Int]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None

    override def \/(lhs: Set[Int], rhs: Set[Int]): Set[Int] = lhs++rhs

    override val bottom: Set[Int] = Set.empty
  }
}