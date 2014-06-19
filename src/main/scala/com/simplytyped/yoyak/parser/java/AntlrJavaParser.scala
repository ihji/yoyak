package com.simplytyped.yoyak.parser.java

import java.io.{InputStream, FileInputStream}
import com.simplytyped.yoyak.parser.{JavaParser, JavaLexer}
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{CommonTokenStream, ANTLRInputStream}

class AntlrJavaParser {
  def parse(filePath: String) : ParseTree = parse(new FileInputStream(filePath))
  def parse(is: InputStream) : ParseTree = {
    val input = new ANTLRInputStream(is)
    val lexer = new JavaLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new JavaParser(tokens)
    val tree = parser.compilationUnit()
    tree
  }
}