package com.simplytyped.yoyak.il

trait Position {
  var startLine: Int
  var endLine: Int
  var startColumn: Int
  var endColumn: Int
  var fileName: String
}

object Position {

  class SourceInfo(var startLine: Int, var endLine: Int, var startColumn: Int, var endColumn: Int, var fileName: String) extends Position {
    override def toString: String = s"$startLine:$endLine:$startColumn:$endColumn:$fileName"
  }

  object NoSourceInfo extends Position {
    var startLine: Int = 0
    var endLine: Int = 0
    var startColumn: Int = 0
    var endColumn: Int = 0
    var fileName: String = "N/A"
  }

}