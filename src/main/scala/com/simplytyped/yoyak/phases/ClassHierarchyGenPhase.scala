package com.simplytyped.yoyak.phases

import com.simplytyped.yoyak.Global
import com.simplytyped.yoyak.il.hierarchy.ClassHierarchyBuilder

class ClassHierarchyGenPhase extends Phase {
  override def run(g: Global): Global = {
    assert(g.pgm.nonEmpty)
    val chBuilder = new ClassHierarchyBuilder
    val ch = chBuilder.build(g.pgm.get)
    g.copy(classHierarchy = Some(ch))
  }
}
