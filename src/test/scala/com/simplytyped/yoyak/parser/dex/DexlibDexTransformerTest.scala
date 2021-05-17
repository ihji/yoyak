package com.simplytyped.yoyak.parser.dex

import org.scalatest.{Matchers, FunSuite}

class DexlibDexTransformerTest extends FunSuite with Matchers {
  test("simple transformation: sample-app.apk") {
    val dexFile = DexlibDexParser.loadDexFile("test/apk/sample-app.apk")
    val pgm     = (new DexlibDexTransformer).translate(dexFile)

    import com.simplytyped.yoyak.il.cfg.CommonILToCFG
    import com.simplytyped.yoyak.analysis.StringAnalysis
    import com.simplytyped.yoyak.il.PrettyPrinter
    val mtd = pgm.findByMethodName("argTest").head
    val cfg = new CommonILToCFG().transform(mtd)
    //println(new PrettyPrinter().toString(mtd))

    val result = new StringAnalysis(cfg).run()

    //println(result)

    pgm.findByMethodName("argTest").nonEmpty should be(true)
  }
}
