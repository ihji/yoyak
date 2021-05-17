package com.simplytyped.yoyak.phases

import com.simplytyped.yoyak.{Global, Options}
import com.simplytyped.yoyak.parser.dex.{DexlibDexParser, DexlibDexTransformer}

class DexParserPhase extends Phase {
  override def run(g: Global): Global = {
    assert(Options.g.target_apk.nonEmpty)
    val target  = Options.g.target_apk.get
    val dexFile = DexlibDexParser.loadDexFile(target)
    val pgm     = (new DexlibDexTransformer).translate(dexFile)
    g.copy(pgm = Some(pgm))
  }
}
