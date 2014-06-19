package com.simplytyped.yoyak.parser.java

import java.io.{ByteArrayInputStream, InputStream}

import org.scalatest.{Matchers, FunSuite}

class AntlrJavaParserTest extends FunSuite with Matchers {
  import AntlrJavaParserTest._
  test("can parser basic java file") {
    val parser = new AntlrJavaParser
    val tree = parser.parse(toStream(simple))
    // TODO: check if it's correct
  }
}

object AntlrJavaParserTest {
  val simple =
    """
      |class Test {
      |    void foo() {
      |        System.out.println("hello");
      |    }
      |}
    """.stripMargin
  def toStream(str: String) : InputStream = new ByteArrayInputStream(str.getBytes)
}