package com.simplytyped.yoyak

object Main {
  def main(args: Array[String]) {
    new OptionParser().parse(Options.g,args)
    println(Options.g.target_apk)
  }
}
