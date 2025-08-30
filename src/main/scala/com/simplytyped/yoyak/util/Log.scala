package com.simplytyped.yoyak.util

import org.apache.logging.log4j.LogManager

object Log {
  val log = LogManager.getLogger("yoyak")
  def error(msg: => String): Unit = {
    if (log.isErrorEnabled) log.error(msg)
  }
}
