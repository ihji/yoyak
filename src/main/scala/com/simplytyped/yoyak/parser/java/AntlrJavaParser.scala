package com.simplytyped.yoyak.parser.java

import java.io.{InputStream, FileInputStream}
import com.simplytyped.yoyak.parser.JavaParser.CompilationUnitContext
import com.simplytyped.yoyak.parser.{JavaParser, JavaLexer}
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}

class AntlrJavaParser {
  var rawParser: Option[JavaParser] = None
  def parse(filePath: String): CompilationUnitContext =
    parse(new FileInputStream(filePath))
  def parse(is: InputStream): CompilationUnitContext = {
    val input  = new ANTLRInputStream(is)
    val lexer  = new JavaLexer(input)
    val tokens = new CommonTokenStream(lexer)
    rawParser = Some(new JavaParser(tokens))
    val unit = rawParser.get.compilationUnit()
    unit
  }
}
