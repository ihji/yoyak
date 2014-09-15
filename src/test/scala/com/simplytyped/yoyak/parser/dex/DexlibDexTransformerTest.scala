package com.simplytyped.yoyak.parser.dex

import com.simplytyped.yoyak.il.PrettyPrinter
import org.scalatest.{Matchers, FunSuite}

class DexlibDexTransformerTest extends FunSuite with Matchers {
  test("simple transformation: sample-app.apk") {
    val dexFile = DexlibDexParser.loadDexFile("test/apk/sample-app.apk")
    val pgm = (new DexlibDexTransformer).translate(dexFile)
    PrettyPrinter.printByMethodName("argTest",pgm)
  }
}
