package com.simplytyped.yoyak.phases

import com.simplytyped.yoyak.Global

class PhaseDriver(firstPhase: Phase) {
  def run(g: Global) : Global = {
    var currentPhase = firstPhase
    var currentGlobal = currentPhase.run(g)
    while(currentPhase.hasNext) {
      currentPhase = currentPhase.next
      currentGlobal = currentPhase.run(currentGlobal)
    }
    currentGlobal
  }
}
