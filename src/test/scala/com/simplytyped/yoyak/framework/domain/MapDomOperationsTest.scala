package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.Galois.SetAbstraction
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MapDomOperationsTest extends AnyFunSuite with Matchers {
  private type SetInt = SetAbstraction[Int]
  private type Domain = MapDom[String, SetInt]

  private val ops = MapDom.ops[String, SetInt]

  private def domain(entries: (String, Set[Int])*): Domain =
    entries.foldLeft(MapDom.empty[String, SetInt]) { (map, entry) =>
      map.update(entry)
    }

  private def contents(map: Domain): Map[String, Set[Int]] =
    map.iterator.toMap

  private def same(left: Domain, right: Domain): Unit =
    contents(left) should be(contents(right))

  private val samples = Seq(
    domain(),
    domain("x" -> Set(1)),
    domain("x" -> Set(2)),
    domain("x" -> Set(1, 2)),
    domain("y" -> Set(1)),
    domain("x" -> Set(1), "y"    -> Set(2)),
    domain("x" -> Set(1, 2), "y" -> Set(1, 2))
  )

  test("weak update adds a value at a previously absent key") {
    val empty   = domain()
    val updated = empty.weakUpdate("x" -> Set(1, 2))

    empty.get("x") should be(Set.empty[Int])
    updated.get("x") should be(Set(1, 2))
    updated.get("missing") should be(Set.empty[Int])
    updated.keySet should be(Set("x"))
    updated.size should be(1)
  }

  test("weak update joins existing values and preserves other keys") {
    val original = domain("x" -> Set(1, 2), "y" -> Set(9))
    val updated  = original.weakUpdate("x" -> Set(2, 3))

    contents(updated) should be(Map("x" -> Set(1, 2, 3), "y" -> Set(9)))
    same(updated.weakUpdate("x" -> Set(2)), updated)
  }

  test("weak update with bottom preserves existing and absent keys") {
    val original = domain("x" -> Set(1))

    same(original.weakUpdate("x" -> Set.empty[Int]), original)
    same(original.weakUpdate("missing" -> Set.empty[Int]), original)
    same(domain().weakUpdate("missing" -> Set.empty[Int]), domain())
  }

  test("weak update leaves its input unchanged") {
    val original = domain("x" -> Set(1), "y" -> Set(2))
    val before   = contents(original)
    val updated  = original.weakUpdate("x" -> Set(3))
    val extended = updated.weakUpdate("z" -> Set(4))

    contents(original) should be(before)
    contents(updated) should be(Map("x" -> Set(1, 3), "y" -> Set(2)))
    extended.get("z") should be(Set(4))
    updated.get("z") should be(Set.empty[Int])
  }

  test("join unions common values and retains disjoint keys immutably") {
    val left        = domain("x" -> Set(1, 2), "left" -> Set(3))
    val right       = domain("x" -> Set(2, 4), "right" -> Set(5))
    val leftBefore  = contents(left)
    val rightBefore = contents(right)
    val joined      = ops.\/(left, right)

    contents(joined) should be(
      Map("x" -> Set(1, 2, 4), "left" -> Set(3), "right" -> Set(5))
    )
    contents(left) should be(leftBefore)
    contents(right) should be(rightBefore)
  }

  test("join has empty as identity and is idempotent") {
    samples.foreach { map =>
      same(ops.\/(map, ops.bottom), map)
      same(ops.\/(ops.bottom, map), map)
      same(ops.\/(map, map), map)
    }
  }

  test("join is commutative and associative") {
    for (left <- samples; right <- samples) {
      same(ops.\/(left, right), ops.\/(right, left))
      samples.foreach { third =>
        same(
          ops.\/(ops.\/(left, right), third),
          ops.\/(left, ops.\/(right, third))
        )
      }
    }
  }

  test("join is the pointwise least upper bound") {
    for (left <- samples; right <- samples) {
      val joined = ops.\/(left, right)
      joined.keySet should be(left.keySet ++ right.keySet)
      joined.keySet.foreach { key =>
        joined.get(key) should be(left.get(key) ++ right.get(key))
      }
      ops.lteqv(left, joined) should be(true)
      ops.lteqv(right, joined) should be(true)
      samples.foreach { upper =>
        if (ops.lteqv(left, upper) && ops.lteqv(right, upper)) {
          ops.lteqv(joined, upper) should be(true)
        }
      }
    }
  }
}
