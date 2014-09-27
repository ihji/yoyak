package com.simplytyped.yoyak.framework.domain

class MapDom[K,V](implicit val valueOps: LatticeOps[V]) {
  private var rawMap = Map.empty[K,V]

  private def add(kv: (K,V)) { if(!valueOps.<=(kv._2,valueOps.bottom)) rawMap += (kv._1 -> kv._2) }
  def update(kv: (K,V)) : MapDom[K,V] = { add(kv); this }
  def weakUpdate(kv: (K,V)) : MapDom[K,V] = {
    add(kv._1 -> valueOps.\/(get(kv._1),kv._2)); this
  }
  def get(k: K) : V = rawMap.getOrElse(k,valueOps.bottom)

  def foldLeft[T](init: T)(f: (T,(K,V)) => T) : T = rawMap.foldLeft(init)(f)
  def size : Int = rawMap.size
  def iterator = rawMap.iterator
}

object MapDom {
  def empty[K,V](implicit valueOps: LatticeOps[V]) = new MapDom[K,V]

  def ops[K,V](implicit valueOps: LatticeOps[V]) = new LatticeOps[MapDom[K,V]] {
    override def <=(lhs: MapDom[K, V], rhs: MapDom[K, V]): Boolean = {
      if(lhs.size > rhs.size) false
      else {
        val lhsIter = lhs.iterator
        var flag = true
        while(flag && lhsIter.hasNext) {
          val (k,v) = lhsIter.next()
          val rValue = rhs.get(k)
          if(!valueOps.<=(v,rValue)) flag = false
        }
        flag
      }
    }

    override def \/(lhs: MapDom[K, V], rhs: MapDom[K, V]): MapDom[K, V] = {
      rhs.foldLeft(lhs) {case (map,(k,v)) => map.weakUpdate(k,v)}
    }

    override val bottom: MapDom[K, V] = MapDom.empty[K,V]
  }
}
