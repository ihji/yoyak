package com.simplytyped.yoyak.domain

import util.parsing.combinator.RegexParsers

/**
 * Created with IntelliJ IDEA.
 * User: ihji
 * Date: 5/13/12
 * Time: 6:34 PM
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class Bool extends AbsDomLike[Bool] {
  def neg : Bool
}
case object Top extends Bool {
  def isTop = true

  def neg = Top

  def <<=(other: Bool) =
    if(other == Top) Some(true) else Some(false)

  def ++(other: Bool) = Top
}
sealed abstract class ConcreteBool extends Bool
case object F extends ConcreteBool {
  def isTop = false

  def neg = T

  def <<=(other: Bool) =
    other match {
      case Top => Some(true)
      case T => None
      case F => Some(true)
      case Bot => Some(false)
    }

  def ++(other: Bool) =
   other match {
     case Top => Top
     case T => Top
     case F => F
     case Bot => F
   }
}
case object T extends ConcreteBool {
  def isTop = false

  def neg = F

  def <<=(other: Bool) =
    other match {
      case Top => Some(true)
      case T => Some(true)
      case F => None
      case Bot => Some(false)
    }

  def ++(other: Bool) =
    other match {
      case Top => Top
      case T => T
      case F => Top
      case Bot => T
    }
}
case object Bot extends Bool {
  def isTop = false

  def neg = Bot

  def <<=(other: Bool) = Some(true)

  def ++(other: Bool) =
    if(other == Bot) Bot else other
}

sealed abstract class PAssignDom extends AbsDomLike[PAssignDom]

case class PAssign(map: MapDom[Int,Bool]) extends PAssignDom {
  def isTop = false

  def get(v: Int) : Bool =
    if(v < 0) map.getOrElse(-v,Bot).neg else map.getOrElse(v,Bot)

  def update(v: Int, b: ConcreteBool) = {
    val newMap = if(v < 0) map.update(-v,b.neg) else map.update(v,b)
    copy(map = newMap)
  }

  def size = map.t.size

  def <<=(other: PAssignDom) =
    other match {
      case _ : PAssignTop => Some(true)
      case PAssign(otherMap) => map.<<=(otherMap)
    }

  def ++(other: PAssignDom) =
    other match {
      case _ : PAssignTop => other
      case PAssign(otherMap) =>
        val newMap = map.++(otherMap)
        if(newMap.t.exists{!_.isInstanceOf[ConcreteBool]})
          PAssignTop(copy(map = newMap))
        else copy(map = newMap)
    }
}

object PAssign {
  val empty = PAssign(MapDom(Map.empty))
  class Parser extends RegexParsers {
    def bool : Parser[Bool] =
      "0" ^^ {_ => F} | "1" ^^ {_ => T}
    def assign : Parser[PAssign] =
      rep1(bool) ^^ {l => PAssign(MapDom((1 to l.length).zip(l).toMap))}
  }
  implicit def str2PAssign(str: String) : PAssign = {
    val parser = new Parser
    val result = parser.parseAll(parser.assign,str)
    if(result.successful) result.get
    else throw new Exception(result.toString)
  }
}

case class PAssignTop(conf: PAssign) extends PAssignDom {
  def isTop = true
  def <<=(other: PAssignDom) =
    if(other.isInstanceOf[PAssignTop]) Some(true) else Some(false)
  def ++(other: PAssignDom) = this
}
