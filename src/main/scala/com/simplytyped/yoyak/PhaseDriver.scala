package com.simplytyped.yoyak

class PhaseDriver(firstPhase: Phase) {
  def run() {
    var currentPhase = firstPhase
    currentPhase.run()
    while(currentPhase.hasNext) {
      currentPhase = currentPhase.next
      currentPhase.run()
    }
  }
}
