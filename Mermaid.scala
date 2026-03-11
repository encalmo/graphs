package org.encalmo.data

object Mermaid {

  enum Direction(val symbol: String) {
    case LeftToRight extends Direction("LR")
    case RightToLeft extends Direction("RL")
    case TopToBottom extends Direction("TB")
    case BottomToTop extends Direction("BT")
  }

  def renderGraph[N](graph: Graph[N], direction: Direction = Direction.TopToBottom): String = {
    val nodes = graph.nodes.map(n => s"    $n").mkString("\n")
    val edges = graph.edges.map(e => s"    ${e._1}-->${e._2}").mkString("\n")
    s"graph ${direction.symbol}\n$nodes\n$edges"
  }

  def renderGraph[N](
      graph: Graph[N],
      classDef: String => String,
      nodeClass: N => String,
      edgeType: ((N, N)) => String,
      direction: Direction
  ): String = {
    val nodesAndClasses = graph.nodes
      .map(n => (n, nodeClass(n)))

    val edges = graph.edges
      .map(e => s"    ${e._1}${edgeType(e)}${e._2}")
      .mkString("\n")

    s"graph ${direction.symbol}\n" +
      s"${nodesAndClasses.map((_, c) => s"    classDef $c ${classDef(c)}").distinct.mkString("\n")}\n"
      + s"${nodesAndClasses.map((n, c) => s"    $n:::$c").mkString("\n")}\n"
      + edges
  }

  def renderStateDiagramV2[N](graph: Graph[N], direction: Direction = Direction.TopToBottom): String = {
    val roots = Graph.rootsOf(graph).map(r => s"    [*] --> $r").mkString("", "\n", "\n")
    val leaves = Graph.leavesOf(graph).map(l => s"    $l --> [*]").mkString("\n", "\n", "")
    val edges = graph.edges.map(e => s"    ${e._1} --> ${e._2}").mkString("\n")

    s"stateDiagram-v2\n    direction ${direction.symbol}\n"
      + roots
      + edges
      + leaves
  }

  def renderStateDiagramV2[N](
      graph: Graph[N],
      starts: Iterable[N],
      ends: Iterable[N],
      classDefs: Map[String, String],
      nodeClass: N => String,
      direction: Direction
  ): String = {
    val roots = starts.map(r => s"    [*] --> $r:::${nodeClass(r)}").mkString("", "\n", "\n")
    val leaves = ends.map(l => s"    $l:::${nodeClass(l)} --> [*]").mkString("\n", "\n", "")
    val edges =
      graph.edges.map(e => s"    ${e._1}:::${nodeClass(e._1)} --> ${e._2}:::${nodeClass(e._2)}").mkString("\n")

    s"stateDiagram-v2\n    direction ${direction.symbol}\n"
      + classDefs.map((k, v) => s"    classDef $k $v").mkString("\n")
      + "\n"
      + roots
      + edges
      + leaves
  }

}
