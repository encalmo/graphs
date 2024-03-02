package org.encalmo.data

import scala.io.Source
import scala.language.implicitConversions

class GraphSpec extends munit.FunSuite {

  val graph1 = Graph[Int](
    1 -> Seq(2, 3),
    2 -> Seq(3),
    3 -> Seq(4),
    4 -> Seq()
  )

  /* cyclic, connected components */
  val graph2 = Graph[Int](
    1 -> Seq(2, 3, 6),
    2 -> Seq(3),
    3 -> Seq(4),
    4 -> Seq(5, 4),
    5 -> Seq(1, 3),
    6 -> Seq(7, 10),
    7 -> Seq(8),
    8 -> Seq(9, 10),
    9 -> Seq(6),
    10 -> Seq(6, 7),
    11 -> Seq(12, 7),
    12 -> Seq(13),
    13 -> Seq(11)
  )

  /* acyclic, weighted */
  val graph3 = Graph[Int, Int](
    1 -> Seq((2, 1), (3, 2), (4, 3)),
    2 -> Seq((3, 3), (5, 4)),
    3 -> Seq((4, 2), (5, 2)),
    4 -> Seq((5, 3)),
    5 -> Seq()
  )

  /* acyclic */
  val graph4 = Graph[Int](
    3 -> Seq(6),
    4 -> Seq(2),
    5 -> Seq(2, 3, 4, 6),
    6 -> Seq(),
    2 -> Seq(3, 6)
  )

  val graph5 = Graph[Int](
    0 -> Seq(1, 3),
    1 -> Seq(0, 2, 3),
    2 -> Seq(1, 3),
    3 -> Seq(0, 1, 2)
  )

  val graph6 = Graph[Int](
    0 -> Seq(1, 2, 3),
    1 -> Seq(0, 2, 3),
    2 -> Seq(0, 1, 3),
    3 -> Seq(0, 1, 2)
  )

  lazy val veryLargeGraph = Graph.readFromEdgeListFile(Source.fromResource("SCC.txt"))
  lazy val weightedGraph = Graph.readFromAdjacentWeightListFile(Source.fromResource("dijkstraData.txt"))
  lazy val minCutGraph = Graph.readFromAdjacentListFile(Source.fromResource("graph1.txt"))

  test("should have nodes and edges") {
    assert(graph1.nodes.size == 4, "graph nodes count should be 4")
    assert(graph1.edges.size == 4, "graph edges count should be 4")
  }

  test("should have hardCopyReversed graph") {
    val reverse = graph1.reverse
    val redges = reverse.edges
    val reversed2 = reverse.reverse
    val radjacentOf1 = reverse.adjacent(1)
    val radjacentOf4 = reverse.adjacent(4)
    assert(reverse.nodes.size == 4, "reversed graph nodes count should be 4")
    assert(redges.size == 4, "reversed graph edges count should be 4")
    assert(reversed2 == graph1, "twice reversed graph should be the same")
    assert(radjacentOf1.isEmpty)
    assert(radjacentOf4.sameElements(Seq(3)))
  }

  test("should search graph with dfs") {
    val graph = graph2
    var counter = 0
    Graph.dfs(
      graph,
      new Graph.DfsVisitor[Int] {
        override def before(node: Int) = {
          counter = counter + 1
        }
      }
    )
    assert(counter == graph.nodesCount, s"counter should be ${graph.nodesCount} but is $counter")
  }

  test("should find strongly connected components") {
    val graph = graph2
    val result = Graph.findStronglyConnectedComponents(graph)
    assert(result.size == 3, s"should be 3 but is ${result.size}")
  }

  test("should read adjacent list graph from file") {
    assert(minCutGraph.nodesCount == 200)
    assert(minCutGraph.adjacent(82).size == 27)
  }

  test("should read adjacent-weight list graph from file") {
    assert(weightedGraph.nodesCount == 200)
    assert(weightedGraph.weight(200, 108) == 9976)
    assert(weightedGraph.adjacent(31).size == 21)
  }

  test("should breath-first search the very large graph - sccGraph") {
    var counter = 0
    Graph.bfs(veryLargeGraph, { (n: Int) => counter = counter + 1 })
    assert(counter == veryLargeGraph.nodesCount, s"should be ${veryLargeGraph.nodesCount} but is ${counter}")
  }

  test("should depth-first search the weighted graph - dijkstraGraph") {
    var counter = 0
    Graph.dfs(
      weightedGraph,
      new Graph.DfsVisitor[Int] {
        override def before(node: Int) = {
          counter = counter + 1
        }
      }
    )
    assert(counter == weightedGraph.nodesCount, s"should be ${weightedGraph.nodesCount} but is ${counter}")
  }

  test("should depth-first search the very large graph - sccGraph") {
    var counter = 0
    Graph.dfs(
      veryLargeGraph,
      new Graph.DfsVisitor[Int] {
        override inline def before(node: Int) = {
          counter = counter + 1
        }
      }
    )
    assert(counter == veryLargeGraph.nodesCount, s"should be ${veryLargeGraph.nodesCount} but is ${counter}")
  }

  test("should find cycles - graph2") {
    val cycles = Graph.findCycles(graph2)
    assert(cycles.size == 6)
  }

  test("should find cycles - graph3") {
    val cycles = Graph.findCycles(graph3)
    assert(cycles.isEmpty)
  }

  test("should check cycles") {
    assert(Graph.hasCycles(graph2))
    assert(!Graph.hasCycles(graph3))
  }

  test("should sort topologically - graph3") {
    val order = Graph.sortTopologically(graph3)
    assert(order.sameElements(Seq(1, 2, 3, 4, 5)))
  }

  test("should sort topologically - graph4") {
    val order = Graph.sortTopologically(graph4)
    assert(order.sameElements(Seq(5, 4, 2, 3, 6)), s"wrong order $order")
  }

  test("should compute shortest path - graph3") {
    val (distance, path) = Graph.findShortestPath(graph3, 1, 5)
    assert(distance == 4, s"should be 4 but is $distance : $path")
    assertEquals(path, List((1, 3), (3, 5)), s"$path")
  }

  test("should compute shortest path - dijkstraData") {
    assert(weightedGraph.nodesCount == 200)
    assert(weightedGraph.weight(200, 108) == 9976)
    assert(weightedGraph.adjacent(31).size == 21)
    val path1 = Graph.findShortestPath(weightedGraph, 1, 197)
    assertEquals(path1, (3068, List((1, 114), (114, 103), (103, 110), (110, 197))))
    val path2 = Graph.findShortestPath(weightedGraph, 1, 115)
    assertEquals(path2, (2399, List((1, 80), (80, 115))), s"$path2")
  }

  test("should compute all shortest paths - graph3") {
    val distance = Graph.findShortestPaths(graph3, 1)
    assert(distance.size == 5)
    assertEquals(distance, Map(1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4))
  }

  test("should compute all shortest paths - dijkstraData") {
    assert(weightedGraph.nodesCount == 200)
    assert(weightedGraph.weight(200, 108) == 9976)
    assert(weightedGraph.adjacent(31).size == 21)
    val distance = Graph.findShortestPaths(weightedGraph, 1)
    val nodes = Seq(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)
    val result = nodes map distance
    assertEquals(result, List(2599, 2610, 2947, 2052, 2367, 2399, 2029, 2442, 2505, 3068))
  }

  test("should merge nodes") {
    val g1 = Graph.mergeNodes(graph5, 1, 0)
    assert(!g1.contains(0))
    assert(g1.contains(1))
    val g2 = Graph.mergeNodes(graph6, 2, 1)
    assert(!g2.contains(1))
    assert(g2.contains(2))
  }

  test("should find min cut count") {
    var count = Integer.MAX_VALUE
    for (i <- 1 to 27) {
      count = Math.min(Graph.randomCutCount(minCutGraph), count)
    }
    assert(count == 17)
  }

  test("should find strongly connected components - scc") {
    val result = Graph.findStronglyConnectedComponents(veryLargeGraph)
    val ten: Seq[Int] = (result.take(10).map(_.size)).toSeq
    assert(ten.sameElements(Seq(434821, 968, 459, 313, 211, 205, 197, 177, 162, 152)))
  }

}
