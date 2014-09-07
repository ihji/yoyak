package com.simplytyped.yoyak.parser.dex

import org.jf.dexlib2.DexFileFactory
import org.jf.dexlib2.dexbacked.DexBackedDexFile

object DexlibDexParser {
  val defaultAPIVersion = 15
  def loadDexFile(path: String) : DexBackedDexFile = {
    DexFileFactory.loadDexFile(path, defaultAPIVersion)
  }
}
