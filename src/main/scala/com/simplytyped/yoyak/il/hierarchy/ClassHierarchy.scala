package com.simplytyped.yoyak.il.hierarchy

import com.simplytyped.yoyak.graph.{ImmutableGraphLike, NodeLike, EdgeLike}
import com.simplytyped.yoyak.il.CommonIL.ClassName
import com.simplytyped.yoyak.il.hierarchy.ClassHierarchy.{
  Inheritance,
  TypeDef,
  HierarchyGraph
}

class ClassHierarchy {
  private var graph = HierarchyGraph.empty

  def extendsFrom(
      childClass: ClassName,
      parentClass: ClassName
  ): ClassHierarchy = {
    graph = graph.addEdge(TypeDef(childClass), TypeDef(parentClass))
    this
  }

  def implementWith(
      childClass: ClassName,
      superInterface: ClassName
  ): ClassHierarchy = {
    graph = graph.addEdge(TypeDef(childClass), TypeDef(superInterface))
    this
  }

  def isExtendsFrom(childClass: ClassName, parentClass: ClassName): Boolean = {
    graph.edges(Inheritance(TypeDef(childClass), TypeDef(parentClass)))
  }

  def isImplementWith(
      childClass: ClassName,
      superInterface: ClassName
  ): Boolean = {
    graph.edges(Inheritance(TypeDef(childClass), TypeDef(superInterface)))
  }

  override def toString: String = graph.toString
}

object ClassHierarchy {
  case class HierarchyGraph(
      nodes: Set[TypeDef],
      edges: Set[Inheritance],
      nexts: Map[TypeDef, Set[Inheritance]],
      prevs: Map[TypeDef, Set[Inheritance]]
  ) extends ImmutableGraphLike[TypeDef, Inheritance, HierarchyGraph] {
    def newEdge(from: TypeDef, to: TypeDef): Inheritance = Inheritance(from, to)
    def builder(
        nodes: Set[TypeDef],
        edges: Set[Inheritance],
        nexts: Map[TypeDef, Set[Inheritance]],
        prevs: Map[TypeDef, Set[Inheritance]]
    ): HierarchyGraph = {
      HierarchyGraph(nodes, edges, nexts, prevs)
    }
  }
  object HierarchyGraph {
    val empty = HierarchyGraph(Set.empty, Set.empty, Map.empty, Map.empty)
  }

  case class TypeDef(data: ClassName) extends NodeLike[TypeDef] {
    type D = ClassName
  }
  case class Inheritance(from: TypeDef, to: TypeDef) extends EdgeLike[TypeDef] {
    type L = Option[Nothing]
    val label = None
  }
}
