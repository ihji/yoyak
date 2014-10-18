package com.simplytyped.yoyak

import com.simplytyped.yoyak.il.CommonIL.Program
import com.simplytyped.yoyak.il.hierarchy.ClassHierarchy

/* global state */
case class Global(
  pgm : Option[Program] = None,
  classHierarchy : Option[ClassHierarchy] = None
)

object Global {
  val empty = Global(None,None)
}
