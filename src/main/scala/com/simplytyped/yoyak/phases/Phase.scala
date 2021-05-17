package com.simplytyped.yoyak.phases

import com.simplytyped.yoyak.Global

trait Phase {
  private var nextPhase: Option[Phase] = None

  def dependsOn(prevPhase: Phase) { prevPhase.nextPhase = Some(this) }
  def hasNext: Boolean = nextPhase.nonEmpty
  def next             = nextPhase.get

  def run(g: Global): Global
}
