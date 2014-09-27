package com.simplytyped.yoyak.framework.domain

class MapDom[K,V](implicit val valueOps: LatticeOps[V]) {
  private var rawMap = Map.empty[K,V]

  def update(kv: (K,V)) : MapDom[K,V] = { rawMap += (kv._1 -> kv._2); this }
  def weakUpdate(kv: (K,V)) : MapDom[K,V] = {
    rawMap += (kv._1 -> valueOps.\/(get(kv._1),kv._2)); this
  }
  def get(k: K) : V = rawMap.getOrElse(k,valueOps.bottom)
  def foldLeft[T](init: T)(f: (T,(K,V)) => T) : T = rawMap.foldLeft(init)(f)

  implicit val ops = new LatticeOps[MapDom[K,V]] {
    override def <=(lhs: MapDom[K, V], rhs: MapDom[K, V]): Boolean = ???

    override def \/(lhs: MapDom[K, V], rhs: MapDom[K, V]): MapDom[K, V] = {
      rhs.foldLeft(lhs) {case (map,(k,v)) => map.weakUpdate(k,v)}
    }

    override val bottom: MapDom[K, V] = MapDom.empty[K,V]
  }
}

object MapDom {
  def empty[K,V](implicit valueOps: LatticeOps[V]) = new MapDom[K,V]

}
