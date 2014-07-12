package com.simplytyped.yoyak.parser.java

import java.io.{ByteArrayInputStream, InputStream}

object AntlrTestHelper {
  val simple =
    """
      |class Test {
      |    void foo() {
      |        System.out.println("hello");
      |    }
      |}
    """.stripMargin
  val simpleWithParam =
    """
      |class Test {
      |    void foo(int[][] x) {
      |        System.out.println("hello");
      |    }
      |}
    """.stripMargin
  def toStream(str: String) : InputStream = new ByteArrayInputStream(str.getBytes)
}
