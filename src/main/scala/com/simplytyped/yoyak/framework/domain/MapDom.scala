package com.simplytyped.yoyak.framework.domain

import com.simplytyped.yoyak.framework.domain.Galois.GaloisIdentity
import com.simplytyped.yoyak.framework.semantics.Widening

class MapDom[K,V <: Galois : LatticeOps] {
  val valueOps = implicitly[LatticeOps[V]]

  private var rawMap = Map.empty[K,V#Abst]

  override def toString = rawMap.mkString("\n")

  private def add(kv: (K,V#Abst)) : MapDom[K,V] = {
    if(!valueOps.isBottom(kv._2)) {
      val newDom = new MapDom[K,V]
      newDom.rawMap = rawMap + (kv._1 -> kv._2)
      newDom
    } else this
  }
  def update(kv: (K,V#Abst)) : MapDom[K,V] = add(kv)
  def weakUpdate(kv: (K,V#Abst)) : MapDom[K,V] = add(kv._1 -> valueOps.\/(get(kv._1),kv._2))
  def get(k: K) : V#Abst = rawMap.getOrElse(k,valueOps.bottom)
  def remove(k: K) : MapDom[K,V] = {
    if(rawMap.get(k).nonEmpty) {
      val newDom = new MapDom[K,V]
      newDom.rawMap = rawMap - k
      newDom
    } else this
  }

  def foldLeft[T](init: T)(f: (T,(K,V#Abst)) => T) : T = rawMap.foldLeft(init)(f)
  def size : Int = rawMap.size
  def iterator = rawMap.iterator
  def keySet = rawMap.keySet
  def head = rawMap.head
}

object MapDom {
  def empty[K,V <: Galois : LatticeOps] = new MapDom[K,V]

  def widening[K,V <: Galois : LatticeOps : Widening] = new Widening[GaloisIdentity[MapDom[K,V]]] {
    val widening = implicitly[Widening[V]]
    override def <>(x: MapDom[K, V], y: MapDom[K, V]) : MapDom[K, V] = {
      val newDom = new MapDom[K,V]
      newDom.rawMap = y.rawMap.map{
        case (k,v) =>
          val oldV = x.get(k)
          val newV = widening.<>(oldV,v)
          (k,newV)
      }
      newDom
    }
  }

  def ops[K,V <: Galois : LatticeOps] = new LatticeOps[GaloisIdentity[MapDom[K,V]]] {
    val valueOps = implicitly[LatticeOps[V]]

    override def partialCompare(lhs: MapDom[K, V], rhs: MapDom[K, V]): Double = {
      // short cuts
      if(lhs.size == 0 && rhs.size == 0) return 0.0
      if(lhs.size == 0 && rhs.size > 0) return -1.0
      if(rhs.size == 0 && lhs.size > 0) return 1.0

      if(lhs.size < rhs.size) {
        val lhsIter = lhs.iterator
        var flag = true
        while(flag && lhsIter.hasNext) {
          val (k,v) = lhsIter.next()
          val rValue = rhs.get(k)
          val order = valueOps.partialCompare(v,rValue)
          if(order.isNaN || order > 0) flag = false
        }
        if(flag) -1.0 else Double.NaN
      } else if(lhs.size > rhs.size){
        val rhsIter = rhs.iterator
        var flag = true
        while(flag && rhsIter.hasNext) {
          val (k,v) = rhsIter.next()
          val lValue = lhs.get(k)
          val order = valueOps.partialCompare(v,lValue)
          if(order.isNaN || order > 0) flag = false
        }
        if(flag) 1.0 else Double.NaN
      } else {
        if(lhs.keySet != rhs.keySet) Double.NaN
        else {
          // lhs.size != 0 && rhs.size != 0
          // find initial order
          val lhsIter = lhs.iterator
          var order = 0.0
          while(!order.isNaN && order == 0 && lhsIter.hasNext) {
            val (k, lv) = lhsIter.next()
            val rv = rhs.get(k)
            order = valueOps.partialCompare(lv, rv)
          }
          if(!order.isNaN && order == 0 && !lhsIter.hasNext) 0.0 // equal
          else if(order.isNaN) Double.NaN
          else {
            // order != 0
            if(order < 0) {
              // assume: lhs < rhs
              // this logic is copied from above
              var flag = true
              while(flag && lhsIter.hasNext) {
                val (k,lValue) = lhsIter.next()
                val rValue = rhs.get(k)
                val order = valueOps.partialCompare(lValue,rValue)
                if(order.isNaN || order > 0) flag = false
              }
              if(flag) -1.0 else Double.NaN
            }
            else {
              // assume: lhs > rhs
              // this logic is copied from above
              var flag = true
              while(flag && lhsIter.hasNext) {
                val (k,lValue) = lhsIter.next()
                val rValue = rhs.get(k)
                val order = valueOps.partialCompare(rValue,lValue)
                if(order.isNaN || order > 0) flag = false
              }
              if(flag) 1.0 else Double.NaN
            }
          }
        }
      }
    }

    override def \/(lhs: MapDom[K, V], rhs: MapDom[K, V]): MapDom[K, V] = {
      rhs.foldLeft(lhs) {case (map,(k,v)) => map.weakUpdate(k,v)}
    }

    override val bottom: MapDom[K, V] = MapDom.empty[K,V]
  }
}
