package org.encalmo.data

import scala.io.Source
import scala.language.implicitConversions

class MermaidSpec extends munit.FunSuite {

  test("should render a simple graph") {
    val graph = Graph[Int](
      1 -> Seq(2, 3),
      2 -> Seq(3),
      3 -> Seq(4),
      4 -> Seq(2)
    )
    val mermaid = Mermaid.renderGraph(graph, Mermaid.Direction.LeftToRight)
    assertEquals(
      mermaid,
      """|graph LR
         |    1
         |    2
         |    3
         |    4
         |    1-->2
         |    1-->3
         |    2-->3
         |    3-->4
         |    4-->2""".stripMargin
    )
  }

  test("should render a graph with classes") {
    type Node = Int | String
    val graph = Graph[Node](
      1 -> Seq(2, 3),
      2 -> Seq(3),
      4 -> Seq("c"),
      "a" -> Seq(4),
      "b" -> Seq(1, "a"),
      "c" -> Seq("b", 2)
    )
    val mermaid = Mermaid.renderGraph(
      graph,
      Map(
        "int" -> "fill:#2058FF,stroke:#2058FF,color:#fff",
        "string" -> "fill:#FF00BF,stroke:#FF00BF,color:#fff"
      ),
      {
        case n: Int    => "int"
        case n: String => "string"
      },
      {
        case (n1: Int, n2: Int)       => "-->"
        case (n1: String, n2: String) => "==>"
        case (n1: Int, n2: String)    => "-->"
        case (n1: String, n2: Int)    => "-.->"
      },
      Mermaid.Direction.TopToBottom
    )
    assertEquals(
      mermaid,
      """|graph TB
         |    classDef int fill:#2058FF,stroke:#2058FF,color:#fff
         |    classDef string fill:#FF00BF,stroke:#FF00BF,color:#fff
         |    1:::int
         |    a:::string
         |    2:::int
         |    b:::string
         |    c:::string
         |    4:::int
         |    1-->2
         |    1-->3
         |    a-.->4
         |    2-->3
         |    b-.->1
         |    b==>a
         |    c==>b
         |    c-.->2
         |    4-->c""".stripMargin
    )
  }

  test("should render a state diagram v2") {
    val graph = Graph[Int](
      1 -> Seq(3, 4),
      2 -> Seq(3),
      3 -> Seq(4),
      4 -> Seq(3, 5),
      5 -> Seq()
    )
    val mermaid = Mermaid.renderStateDiagramV2(graph)
    assertEquals(
      mermaid,
      """|stateDiagram-v2
         |    direction TB
         |    [*] --> 1
         |    [*] --> 2
         |    1 --> 3
         |    1 --> 4
         |    2 --> 3
         |    3 --> 4
         |    4 --> 3
         |    4 --> 5
         |    5 --> [*]""".stripMargin
    )
  }

  test("should render a state diagram v2 with classes") {
    val graph = Graph[Int](
      1 -> Seq(3, 4),
      2 -> Seq(3),
      3 -> Seq(4),
      4 -> Seq(3, 5),
      5 -> Seq()
    )
    val mermaid = Mermaid.renderStateDiagramV2(
      graph,
      Seq(1, 2),
      Seq(5),
      Map(
        "foo" -> "fill:#2058FF,stroke:#2058FF,color:#fff",
        "bar" -> "fill:#FF00BF,stroke:#FF00BF,color:#fff"
      ),
      {
        case 1 => "foo"
        case 2 => "foo"
        case 3 => "bar"
        case 4 => "bar"
        case 5 => "foo"
      },
      Mermaid.Direction.TopToBottom
    )
    assertEquals(
      mermaid,
      """|stateDiagram-v2
         |    direction TB
         |    classDef foo fill:#2058FF,stroke:#2058FF,color:#fff
         |    classDef bar fill:#FF00BF,stroke:#FF00BF,color:#fff
         |    [*] --> 1:::foo
         |    [*] --> 2:::foo
         |    1:::foo --> 3:::bar
         |    1:::foo --> 4:::bar
         |    2:::foo --> 3:::bar
         |    3:::bar --> 4:::bar
         |    4:::bar --> 3:::bar
         |    4:::bar --> 5:::foo
         |    5:::foo --> [*]""".stripMargin
    )
  }

}
