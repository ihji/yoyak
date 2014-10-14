package com.simplytyped.yoyak

import com.simplytyped.yoyak.il.CommonIL.Program

/* global state */
case class Global(
  pgm : Option[Program] = None
)

object Global {
  val empty = Global(None)
}
