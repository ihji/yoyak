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

    override def <=(lhs: MapDom[K, V], rhs: MapDom[K, V]): Option[Boolean] = {
      // short cuts
      if(lhs.size == 0 && rhs.size >= 0) return Some(true)
      if(rhs.size == 0 && lhs.size > 0) return Some(false)

      if(lhs.size < rhs.size) {
        val lhsIter = lhs.iterator
        var flag = true
        while(flag && lhsIter.hasNext) {
          val (k,v) = lhsIter.next()
          val rValue = rhs.get(k)
          val order = valueOps.<=(v,rValue)
          if(order.isEmpty || !order.get) flag = false
        }
        if(flag) Some(true) else None
      } else if(lhs.size > rhs.size){
        val rhsIter = rhs.iterator
        var flag = true
        while(flag && rhsIter.hasNext) {
          val (k,v) = rhsIter.next()
          val lValue = lhs.get(k)
          val order = valueOps.<=(v,lValue)
          if(order.isEmpty || !order.get) flag = false
        }
        if(flag) Some(false) else None
      } else {
        if(lhs.keySet != rhs.keySet) None
        else {
          // lhs.size != 0 && rhs.size != 0
          // find initial order
          val lhsIter = lhs.iterator
          var initOrderLR : Option[Boolean] = Some(true)
          var initOrderRL : Option[Boolean] = Some(true)
          while(initOrderLR.nonEmpty && initOrderLR.get && initOrderRL.nonEmpty && initOrderRL.get && lhsIter.hasNext) {
            val (k, lv) = lhsIter.next()
            val rv = rhs.get(k)
            initOrderLR = valueOps.<=(lv, rv)
            initOrderRL = valueOps.<=(rv, lv)
          }
          if(initOrderLR.nonEmpty && initOrderLR.get && initOrderRL.nonEmpty && initOrderRL.get && !lhsIter.hasNext) Some(true) // equal
          else if(initOrderLR.isEmpty || initOrderRL.isEmpty) None
          else {
            // assume both initOrderLR and initOrderRL are not Some(false). logically impossible case: lv > rv && lv < rv
            if(initOrderLR.get) {
              // assume: lhs <= rhs
              // this logic is copied from above
              var flag = true
              while(flag && lhsIter.hasNext) {
                val (k,lValue) = lhsIter.next()
                val rValue = rhs.get(k)
                val order = valueOps.<=(lValue,rValue)
                if(order.isEmpty || !order.get) flag = false
              }
              if(flag) Some(true) else None
            }
            else {
              // assume: lhs >= rhs
              // this logic is copied from above
              var flag = true
              while(flag && lhsIter.hasNext) {
                val (k,lValue) = lhsIter.next()
                val rValue = rhs.get(k)
                val order = valueOps.<=(rValue,lValue)
                if(order.isEmpty || !order.get) flag = false
              }
              if(flag) Some(false) else None
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
