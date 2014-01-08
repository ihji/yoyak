package com.simplytyped.yoyak.domain

case class MapDom[D1, D2 <: AbsDomLike[D2]](t : Map[D1, D2])
  extends AbsDomLike[MapDom[D1, D2]] {

  override def toString = "(%s):%s".format(t.size,t.mkString(", "))

  def isTop = false

  def get(key: D1): Option[D2] = t.get(key)

  def getOrElse(key: D1, v: D2): D2 = t.getOrElse(key,v)

  def update(key: D1, elem: D2): MapDom[D1, D2] = copy(t = t+(key->elem))

  def weakUpdate(key: D1, elem: D2): MapDom[D1, D2] =
    get(key) match {
      case Some(elem_) => update(key, elem ++ elem_)
      case None => update(key, elem)
    }

  def <<=(other: MapDom[D1, D2]): Option[Boolean] = {
    if(t.size > other.t.size) {
      if(other.t.foldLeft(true){case (b,(k,v)) => t.get(k).flatMap{v <<= _}.getOrElse(false) && b}) Some(false)
      else None
    } else if(t.size < other.t.size) {
      if(t.foldLeft(true){case (b,(k,v)) => other.t.get(k).flatMap{v <<= _}.getOrElse(false) && b}) Some(true)
      else None
    } else {
      val (lt,rt) = t.foldLeft(true,true){
        case ((l,r),(k,v)) =>
          val (l$,r$) = other.t.get(k).map{x =>
            ((v <<= x).getOrElse(false), (x <<= v).getOrElse(false))}.getOrElse(false,false)
          (l && l$, r && r$)
      }
      if(lt && rt) Some(true)
      else if(lt && !rt) Some(true)
      else if(!lt && rt) Some(false)
      else None
    }
  }

  def ++(other: MapDom[D1, D2]) =
    t.foldLeft(other){
        (m, pair) => m.weakUpdate(pair._1, pair._2)
    }
}
