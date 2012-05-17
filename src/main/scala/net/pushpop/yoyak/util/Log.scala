package net.pushpop.yoyak.util

object Log {
  def error(msg: => String) {
    if(Option.debugLevel > 0) println("[ERROR] %s" format msg)
  }
}
