package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.Position.NoSourceInfo

trait Attachable {
  protected var sourcePos: Position = NoSourceInfo
  def setPos(pos: Position): this.type = { sourcePos = pos; this }
  def pos = sourcePos
}
