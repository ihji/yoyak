package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.MapDomTest.SetInt
import org.scalatest.{Matchers, FunSuite}

class MapDomTest extends FunSuite with Matchers {
  test("MapDom ordering (<=) test: unrelated") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3))
    val map2 = MapDom.empty[Int,SetInt].update(2->Set(1,2,3))
    val ordering = MapDom.ops[Int,SetInt]

    ordering.<=(map1,map2) should be (None)
  }
  test("MapDom ordering (<=) test: lhs is bigger") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3,4))
    val map2 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3))
    val ordering = MapDom.ops[Int,SetInt]

    ordering.<=(map1,map2) should be (Some(false))
  }
  test("MapDom ordering (<=) test: lhs is bigger 2") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3)).update(2->Set(1,3,4))
    val map2 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3)).update(2->Set(1))
    val ordering = MapDom.ops[Int,SetInt]

    ordering.<=(map1,map2) should be (Some(false))
  }
  test("MapDom ordering (<=) test: rhs is bigger") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3))
    val map2 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3,4))
    val ordering = MapDom.ops[Int,SetInt]

    ordering.<=(map1,map2) should be (Some(true))
  }
  test("MapDom ordering (<=) test: lhs has more elements") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,SetInt].update(1->Set(1,2)).update(2->Set(2,3,4))
    val map2 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3))
    val ordering = MapDom.ops[Int,SetInt]

    ordering.<=(map1,map2) should be (None)
  }
  test("MapDom ordering (<=) test: rhs has more elements") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3))
    val map2 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3)).update(2->Set(2,3,4))
    val ordering = MapDom.ops[Int,SetInt]

    ordering.<=(map1,map2) should be (Some(true))
  }
  test("MapDom ordering (<=) test: rhs has more elements, but lhs has a bigger element") {
    import MapDomTest.ops
    val map1 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3,4))
    val map2 = MapDom.empty[Int,SetInt].update(1->Set(1,2,3)).update(2->Set(2,3,4))
    val ordering = MapDom.ops[Int,SetInt]

    ordering.<=(map1,map2) should be (None)
  }
}

object MapDomTest {
  class SetInt extends Galois {
    override type Conc = Int
    override type Abst = Set[Int]
  }
  implicit val ops : LatticeOps[SetInt] = new LatticeOps[SetInt] {
    override def <=(lhs: Set[Int], rhs: Set[Int]): Option[Boolean] =
      if(lhs subsetOf rhs) Some(true)
      else if(rhs subsetOf lhs) Some(false)
      else None

    override def \/(lhs: Set[Int], rhs: Set[Int]): Set[Int] = lhs++rhs

    override val bottom: Set[Int] = Set.empty
  }
}