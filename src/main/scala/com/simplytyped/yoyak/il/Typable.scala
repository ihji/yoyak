package com.simplytyped.yoyak.il

import com.simplytyped.yoyak.il.CommonIL.Type

class Typable {
  protected var rawType : Type.ValueType = Type.UnknownType

  def setType(ty: Type.ValueType) : this.type = {rawType = ty; this}
  def ty = rawType
}
