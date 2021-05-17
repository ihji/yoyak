package com.simplytyped.yoyak.il.hierarchy

import com.simplytyped.yoyak.il.CommonIL.Program

class ClassHierarchyBuilder {
  def build(pgm: Program): ClassHierarchy = {
    pgm.classes.foldLeft(new ClassHierarchy) { case (ch, (_, clazz)) =>
      clazz.interfaces.foldLeft(
        ch.extendsFrom(clazz.name, clazz.superClass)
      ) {
        _.implementWith(clazz.name, _)
      }
    }
  }
}
