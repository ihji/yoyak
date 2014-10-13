package com.simplytyped.yoyak

trait Phase {
  private var nextPhase : Option[Phase] = None

  def dependsOn(prevPhase: Phase) { prevPhase.nextPhase = Some(this) }
  def hasNext : Boolean = nextPhase.nonEmpty
  def next = nextPhase.get

  def run()
}
