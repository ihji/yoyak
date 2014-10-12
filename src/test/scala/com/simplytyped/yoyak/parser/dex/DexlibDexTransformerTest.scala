package com.simplytyped.yoyak.parser.dex

import org.scalatest.{Matchers, FunSuite}
//import com.simplytyped.yoyak.analysis.StringAnalysis
//import com.simplytyped.yoyak.il.PrettyPrinter
//import com.simplytyped.yoyak.il.cfg.CommonILToCFG
//import com.simplytyped.yoyak.il.opt.VarSplitting

class DexlibDexTransformerTest extends FunSuite with Matchers {
  test("simple transformation: sample-app.apk") {
    val dexFile = DexlibDexParser.loadDexFile("test/apk/sample-app.apk")
    val pgm = (new DexlibDexTransformer).translate(dexFile)
    pgm.findByMethodName("argTest").nonEmpty should be (true)
//    val toCFG = new CommonILToCFG
//    val cfg = toCFG.transform(pgm.findByMethodName("argTest").head.statements)
//    val varsplitting = new VarSplitting
//    varsplitting.run(cfg)
//    println(new PrettyPrinter().toDot(cfg))
//    val analysis = new StringAnalysis(cfg)
//    println(analysis.run())
  }
}
