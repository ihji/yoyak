package com.simplytyped.yoyak

import com.simplytyped.yoyak.phases._

object Main {
  val dexParser      = new DexParserPhase
  val classHierarchy = new ClassHierarchyGenPhase
  val cfgGen         = new CfgGenPhase
  val varSplitting   = new VarSplittingPhase
  val stringAnalysis = new StringAnalysisPhase

  val androidStringAnalysis = {
    val phases = List(dexParser,classHierarchy,cfgGen,varSplitting,stringAnalysis)
    phases.sliding(2).foreach{list => list(1).dependsOn(list(0))}
    phases.head
  }

  def main(args: Array[String]) {
    new OptionParser().parse(args)
    val driver = new PhaseDriver(androidStringAnalysis)
    val global = Global.empty
    driver.run(global)
  }
}
