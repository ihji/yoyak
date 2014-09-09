package com.simplytyped.yoyak.il

class SourceInfo(var sl: Int, var el: Int, var sc: Int, var ec: Int, var fileName: String) {
  override def toString() : String = s"$sl:$el:$sc:$ec:$fileName"
}

object SourceInfo {
  def makeNewSourceInfo() : SourceInfo = new SourceInfo(sl = 0, el = 0, sc = 0, ec = 0, fileName = "N/A")
  val dummy = makeNewSourceInfo()
}