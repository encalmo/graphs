<a href="https://github.com/encalmo/graphs">![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)</a> <a href="https://central.sonatype.com/artifact/org.encalmo/graphs_3" target="_blank">![Maven Central Version](https://img.shields.io/maven-central/v/org.encalmo/graphs_3?style=for-the-badge)</a> <a href="https://encalmo.github.io/graphs/scaladoc/org/encalmo/data.html" target="_blank"><img alt="Scaladoc" src="https://img.shields.io/badge/docs-scaladoc-red?style=for-the-badge"></a>

# graphs

Scala library for processing graphs.

## Dependencies

   - JVM >= 21
   - [Scala](https://www.scala-lang.org) >= 3.7.4

## Usage

Use with SBT

    libraryDependencies += "org.encalmo" %% "graphs" % "0.12.0"

or with SCALA-CLI

    //> using dep org.encalmo::graphs:0.12.0

## Table of contents

- [Dependencies](#dependencies)
- [Usage](#usage)
- [Motivation](#motivation)
- [Supported Graph Algorithms](#supported-graph-algorithms)
- [Creating a Graph](#creating-a-graph)
      - [Weighted Graphs](#weighted-graphs)
   - [`Graph.apply` Methods](#`graph.apply`-methods)
      - [Constructing a Graph from a Map](#constructing-a-graph-from-a-map)
      - [Constructing a Graph from series of (node, adjacent nodes) pairs](#constructing-a-graph-from-series-of-(node,-adjacent-nodes)-pairs)
      - [Constructing a Graph from a set of nodes and an adjacency function](#constructing-a-graph-from-a-set-of-nodes-and-an-adjacency-function)
      - [Constructing a Graph from a Sequence of Edges](#constructing-a-graph-from-a-sequence-of-edges)
      - [Constructing a Weighted Graph from Node-Weighted Adjacency Lists](#constructing-a-weighted-graph-from-node-weighted-adjacency-lists)
   - [Loading Graphs from Files](#loading-graphs-from-files)
   - [Mutating a Graph](#mutating-a-graph)
- [Computing Properties of a Graph](#computing-properties-of-a-graph)
   - [Nodes and Edges](#nodes-and-edges)
   - [Adjacency and Neighbors](#adjacency-and-neighbors)
   - [Roots and Leaves](#roots-and-leaves)
   - [Predecessors and Ancestors](#predecessors-and-ancestors)
   - [Traversal: BFS and DFS](#traversal:-bfs-and-dfs)
   - [Cycle Detection](#cycle-detection)
   - [Topological Sorting](#topological-sorting)
   - [Strongly Connected Components (SCCs)](#strongly-connected-components-(sccs))
   - [Shortest Paths (Dijkstra)](#shortest-paths-(dijkstra))
- [Graph Traits and Implementations](#graph-traits-and-implementations)
   - [Main Traits](#main-traits)
   - [Built-in concrete implementations](#built-in-concrete-implementations)
   - [How to Choose](#how-to-choose)
- [Mermaid diagrams](#mermaid-diagrams)
      - [Simple graph diagram](#simple-graph-diagram)
      - [Graph diagram with clases and edge types](#graph-diagram-with-clases-and-edge-types)
      - [Simple state diagram](#simple-state-diagram)
      - [State diagram with classes](#state-diagram-with-classes)
- [Project content](#project-content)

## Motivation

Graph algorithms (traversals, shortest paths, strongly connected components, min-cut, etc.) show up in many domains—dependency resolution, scheduling, network analysis, puzzles. This library provides a small, dependency-light, idiomatic Scala API for building and querying directed and undirected graphs, with built-in support for common algorithms and for loading graphs from standard formats (edge lists, adjacency lists). Use it when you need graph processing without pulling in a heavy framework.

## Supported Graph Algorithms

The library implements several essential graph algorithms out of the box. These include:

- **Depth-First Search (DFS)**: Traverse nodes in depth-first order with support for pre/post visit hooks.
- **Breadth-First Search (BFS)**: Traverse nodes layer-by-layer from a starting node.
- **Topological Sort**: Returns a linear ordering of nodes for Directed Acyclic Graphs (DAGs).
- **Cycle Detection**: Check for cycles and obtain all nodes involved in cycles.
- **Strongly Connected Components (SCC)**: Computes all strongly connected components using Kosaraju's algorithm.
- **Shortest Paths (Weighted, Dijkstra)**: Find shortest paths and distances between nodes in graphs with integer edge weights (Dijkstra’s algorithm).
- **Min-Cut (Karger's Algorithm)**: Estimates the minimum cut of an undirected graph.
- **Graph Reversal**: Obtain a reversed version of a directed graph.

## Graph[N] is an interface, not a concrete structure

The type `Graph[N]` in this library is an **interface** (trait) which abstracts over the underlying data structure or representation of the graph. This means that when you use a `Graph[N]`, you are working through a uniform API for querying nodes, edges, adjacency, and running algorithms, **regardless of how the graph is actually stored** internally.

Different concrete implementations back a `Graph[N]`, including:
- **`MapGraph[N]`**: Backed by an immutable `Map[N, Traversable[N]]`.
- **`MutableMapGraph[N]`**: Backed by a mutable map for efficient updates.
- **`GenericGraphImpl[N]`**: Allows you to wrap any node collection and adjacency function as a graph.
- **`WeightedGraphImpl[N, V]`**: Stores additional weight data for weighted graphs.

### Providing Custom `Graph[N]` Implementations

You are not limited to the built-in graph classes—**you can create your own `Graph[N]` implementation by wrapping your existing data structure or class hierarchy**, as long as you supply the required interface methods.

The minimal required interface is:

- `nodes: Traversable[N]` — yields all nodes of the graph
- `adjacent(node: N): Traversable[N]` — yields all nodes directly reachable from `node`
- (Optionally) any other methods of the `Graph[N]` trait you wish to override

#### Wrapping Existing Data Structures

Suppose you already have nodes stored in a collection, and for each node you have a way to get its out-neighbors, you can use `Graph.GenericGraphImpl` to quickly expose them as a `Graph[N]`. Example:

```scala
import org.encalmo.data.Graph

case class MyNode(id: Int, neighbors: Seq[MyNode])
val a = MyNode(1, Nil)
val b = MyNode(2, Nil)
val c = MyNode(3, Nil)

a.neighbors :+= b
b.neighbors :+= c

val allNodes = Seq(a, b, c)

val myGraph: Graph[MyNode] = new Graph.GenericGraphImpl[MyNode](
  allNodes,
  (n: MyNode) => n.neighbors
)
```

Now, `myGraph` supports all the graph algorithms and queries from this library, even though your internal structure is custom.

#### Custom Class Hierarchies

If you have elaborate class hierarchies (e.g., a complex `Node` tree), you can always define your own subclass of `Graph[N]` or directly implement the `GenericGraph[N]` trait, providing logic for enumerating nodes and their adjacency.

Example (using an inner class):

```scala
class MyGraph(nodesSet: Set[Node]) extends Graph.GenericGraph[Node] {
  override def nodes: Traversable[Node] = nodesSet
  override def adjacent(node: Node): Traversable[Node] = node.childNodes
  // Optionally, override more methods for efficiency
}
```

This makes the library a powerful universal tool for querying and transforming *any* graph-like object structures in your codebase, with minimal changes to your domain models.

## Creating a Graph

You can create graphs using the `Graph` companion object's `apply` methods. These support both unweighted (default) and weighted graphs, with intuitive syntax.

A directed graph can be built by specifying each node and the sequence of its outgoing neighbors as key-value pairs:

```scala
import org.encalmo.data.Graph

val g = Graph[Int](
  1 -> Seq(2, 3),
  2 -> Seq(3),
  3 -> Seq(4),
  4 -> Seq()
)
```

This creates a graph with edges 1→2, 1→3, 2→3, and 3→4.

#### Weighted Graphs

For weighted graphs, provide pairs (neighbor, weight) for each outgoing edge:

```scala
val weighted = Graph[Int, Int](
  1 -> Seq((2, 5), (3, 1)), // Edges: 1→2 (weight 5), 1→3 (weight 1)
  2 -> Seq((3, 4)),
  3 -> Seq()
)
```

Here, the first type parameter is the node type, the second is the weight type.

### `Graph.apply` Methods

The library provides convenient `apply` methods to construct graphs in various ways:

#### Constructing a Graph from a Map

If you already have a `Map` whose keys are nodes and whose values are traversable collections of adjacent nodes, you can construct a `MapGraph` directly:

```scala
val nodeMap = Map(
  1 -> Seq(2, 3),
  2 -> Seq(3),
  3 -> Seq(4),
  4 -> Seq()
)
val g: MapGraph[Int] = Graph(nodeMap)
```

This method leverages the `Graph.apply[N](map: Map[N, Traversable[N]])` constructor and returns a `MapGraph[N]` instance that efficiently references your supplied adjacency structure.
 
If the key type is `Int`, you’ll get a specialized performance-optimized `IntMapGraph` under the hood.
 
This is particularly useful when reading from sources that already produce adjacency maps or when transforming data from other libraries.

#### Constructing a Graph from series of (node, adjacent nodes) pairs

You can also build a `MapGraph` directly from a series of node-to-adjacents pairs using varargs:

```scala
val g: MapGraph[Int] = Graph(
  1 -> Seq(2, 3),
  2 -> Seq(3, 4),
  3 -> Seq(4),
  4 -> Seq()
)
```

This syntax is equivalent to passing a `Map` of adjacency lists, but lets you define the edges inline without explicitly assembling the map. The type of each entry is `(N, Traversable[N])`, so you can use any type for the adjacent nodes as long as it is traversable.

This construction uses the method:
```
def apply[N](mappings: (N, Traversable[N])*): MapGraph[N]
```
and efficiently creates a new `MapGraph` (or `IntMapGraph` for integer keys). This approach is especially handy for building small static graphs or for quick, readable test cases.

#### Constructing a Graph from a set of nodes and an adjacency function

For scenarios where you have a collection of nodes (of type `Iterable[N]`) and a function to compute adjacent nodes for each node, you can construct a `GenericGraph` using:

```scala
val nodes: Iterable[Int] = List(1, 2, 3, 4)
val adjacency: Int => Traversable[Int] = {
  case 1 => Seq(2, 3)
  case 2 => Seq(3)
  case 3 => Seq(4)
  case 4 => Seq()
}

val g: Graph[Int] = Graph(nodes, adjacency)
```

This approach is particularly useful when the set of nodes is known, and adjacency relationships are defined procedurally or computed on the fly, rather than derived from a static map or edge list. It is also ideal for dynamically generated graphs, programmatic or algorithmic relationships, and for interoperating with existing APIs/data sources that supply nodes and adjacency logic separately.

The method signature is:
```
def apply[N](nodes: Iterable[N], adjacent: N => Traversable[N]): Graph[N]
```
The resulting graph exposes all standard `Graph` operations, and will call your `adjacent` function to determine the outgoing edges for each node in the provided `nodes` set.

#### Constructing a Graph from a Sequence of Edges

You can also construct a graph from a flat collection of edges, with each edge represented as a tuple `(from, to)`. This is especially useful when your data is already in edge-list format or when you want to build up a graph incrementally from a set of relationships.

Example:

```scala
val edges = Seq(
  (1, 2),
  (1, 3),
  (2, 3),
  (3, 4)
)
val g: MutableMapGraph[Int] = Graph(edges)
```

This uses the following constructor:

```
def apply[N](edges: Traversable[(N, N)]): MutableMapGraph[N]
```

Each pair `(a, b)` in `edges` will create a directed edge from `a` to `b`. The resulting graph is mutable (`MutableMapGraph`), allowing you to add or remove nodes and edges after construction. This method is handy for loading data-driven graphs or parsing edge lists from files or output of other libraries.

#### Constructing a Weighted Graph from Node-Weighted Adjacency Lists

If you need to represent a graph where each edge carries an explicit weight (for example, for shortest path or flow algorithms), you can easily construct such a weighted graph with the following constructor:

```scala
val g: Graph[Int] & Weighted[Int, Double] = Graph(
  1 -> Iterable(2 -> 0.5, 3 -> 1.2),
  2 -> Iterable(3 -> 2.0),
  3 -> Iterable.empty
)
```

This method signature is:
```
def apply[N, V: Numeric](mappings: (N, Iterable[(N, V)])*): Graph[N] & Weighted[N, V]
```

Here:
- Each tuple `(N, Iterable[(N, V)])` associates node `N` with its outgoing edges and the weight of each edge.
- Supported for any numeric type `V` (e.g., `Int`, `Double`, `Float`).
- The resulting graph implements both the `Graph[N]` operations as well as the `Weighted[N, V]` trait, which provides access to the edge weights via `weight(from, to)`.

**Example Usage:**

```scala
val weightedGraph = Graph(
  "A" -> Iterable("B" -> 10, "C" -> 20),
  "B" -> Iterable("C" -> 5),
  "C" -> Iterable.empty[String, Int]
)

val weightAB: Int = weightedGraph.weight("A", "B") // 10
val weightBC: Int = weightedGraph.weight("B", "C") // 5
```

This construction is ideal for small static weighted graphs, test fixtures, or when your data is already available as node-to-adjacent-with-weight mappings. All standard graph operations remain available, and you can query weights as needed via the returned `Weighted` trait.

### Loading Graphs from Files

You can also construct graphs from files in standard formats (e.g., edge lists or adjacency lists):

```scala
val graphFromEdgeList = Graph.readFromEdgeListFile(Source.fromFile("edges.txt"))
val graphFromAdjacencyList = Graph.readFromAdjacentListFile(Source.fromFile("adjlist.txt"))

// For weighted adjacency list:
val weightedGraph = Graph.readFromAdjacentWeightListFile(Source.fromFile("weighted_adjlist.txt"))
```

### Mutating a Graph

By default, the `Graph` API creates immutable graph instances. To perform mutations (add/remove nodes or edges), you can convert the immutable graph to a mutable version using `.mutableCopy`. All the following operations are supported on mutable graphs:

```scala
import org.encalmo.data.Graph

// Create an immutable graph
val g = Graph[Int](
  1 -> Seq(2, 3),
  2 -> Seq(3),
  3 -> Seq()
)

// Get a mutable copy
val mutableGraph = g.mutableCopy

// Add a node
mutableGraph.addNode(4)

// Add an edge from node 3 to node 4
mutableGraph.addEdge(3, 4)

// Remove an edge from node 2 to node 3
mutableGraph.removeEdge(2, 3)

// Remove a node (and all its incident edges)
mutableGraph.removeNode(1)
```

After mutating, you can continue to use the mutable graph, or freeze it back to an immutable instance with `.freeze`:

```scala
val immutableAgain = mutableGraph.freeze
```

**Note:** Mutating shared immutable graphs will not alter the original graph. Mutations only affect the mutable copy.

## Computing Properties of a Graph

The `Graph` API provides various methods to compute important properties and perform common graph algorithms:

### Nodes and Edges

Basic properties to introspect the structure of any graph:

```scala
val g = Graph(1 -> Seq(2, 3), 2 -> Seq(3), 3 -> Seq())
// Number of nodes
val nodeCount = g.nodesCount                 // Int
val nodeSet = g.nodes                        // collection.Set[N]
// Number of edges
val edgeCount = g.edges.size                 // Int
val edgeList = g.edges                       // Iterable[(N, N)]
// Check existence
val hasNode = g.containsNode(2)              // Boolean
val hasEdge = g.containsEdge(1, 3)           // Boolean
```

### Adjacency and Neighbors

Query neighboring nodes and edge weights:

```scala
val neighbors = g.adjacent(1)                  // Iterable[N]
val weight = weightedGraph.weight(1, 3)        // Edge weight between 1 and 3
```

### Roots and Leaves

You can easily obtain the roots (nodes with no predecessors, i.e., no incoming edges) and leaves (nodes with no outgoing edges) of a graph. These are helpful, for example, to identify entry points or terminal nodes in dependency graphs and workflows:

```scala
val g = Graph(1 -> Seq(2, 3), 2 -> Seq(3), 3 -> Seq())
// Find root nodes (no incoming edges)
val roots = Graph.rootsOf(g)      // Returns Set(1)

// Find leaf nodes (no outgoing edges)
val leaves = Graph.leavesOf(g)    // Returns Set(3)
```

- In the example above, node 1 is a root because it has no incoming edges.
- Node 3 is a leaf because it has no outgoing edges.

### Predecessors and Ancestors

You can compute the predecessors (nodes with edges into a given node) and ancestors (all nodes from which you can reach a given node via a path) of a node:

```scala
val preds = g.predecessors(3)      // Returns a Set[N] of immediate predecessors of node 3
val ancestors = g.ancestors(3)     // Returns a Set[N] of all reachable ancestors of node 3
```

For example, with:

```scala
val g = Graph(1 -> Seq(2, 3), 2 -> Seq(3), 3 -> Seq())
```

- `g.predecessors(3)` yields `Set(1, 2)`, since nodes 1 and 2 both have an edge to 3.
- `g.ancestors(3)` yields `Set(1, 2)`, as both can reach 3 via a path.

These are useful for dependency analysis, reachability, and more.

### Traversal: BFS and DFS

Traverse graphs with breadth-first or depth-first strategies:

```scala
// Depth-first search (DFS) visiting every node:
Graph.dfs(g)(new Graph.DfsVisitor[Int] {
  override def before(node: Int) = println(s"DFS visits $node")
})

// Breadth-first search (BFS):
Graph.bfs(g) { node => println(s"BFS visits $node") }
```

### Cycle Detection

Detect cycles and retrieve cyclic nodes:

```scala
val hasCycles = Graph.hasCycles(g2)          // returns true/false
val cycles = Graph.findCycles(g2)            // Returns Vector[N] of nodes in cycles
```

### Topological Sorting

For directed acyclic graphs (DAGs), you can obtain a valid topological ordering:

```scala
val order = Graph.sortTopologically(g3)      // Returns List[N], in topological order
```

### Strongly Connected Components (SCCs)

Identify SCCs in directed graphs:

```scala
val sccs = Graph.findStronglyConnectedComponents(g2) // Vector[Set[N]]
```

### Shortest Paths (Dijkstra)

For weighted graphs (with positive weights), compute shortest paths:

```scala
val (distance, path) = weightedGraph.findShortestPath(1, 5)
// distance: lowest total weight Int
// path: List[(N, N)] representing the sequence of edges

val allDistances = weightedGraph.findShortestPaths(1)
// allDistances: Map[N, Int]
```

## Graph Traits and Implementations

This library provides a set of flexible abstractions and implementations for representing graphs of various types:

### Main Traits

- **Graph[N]**  
  The core trait that defines a graph over nodes of type `N`. Key members include:
  - `nodes: Traversable[N]` &mdash; all nodes in the graph.
  - `adjacent: N => Traversable[N]` &mdash; immediate neighbors for each node.
  - `edges: Traversable[(N, N)]` &mdash; directed edges of the graph.
  - `reverse: Graph[N]` &mdash; the same graph with all edges reversed (for directed graphs).
  - `nodesCount: Int`, `edgesCount: Long` – counts of nodes and edges.

- **GenericGraph[N]**  
  An abstract base for quickly defining graphs by just specifying nodes and the `adjacent` relation. Most methods (like `edges`, `reverse`, etc.) have default implementations.

- **Weighted[N, V]**  
  A trait for weighted graphs, providing a `weight: (N, N) => V` method for edge weights.

- **Mutable[N]**  
  A trait for mutable graphs, extending both the Growable and Shrinkable interfaces for edges. Provides methods to mutate the graph, remove nodes, and transform adjacency lists.

### Built-in concrete implementations

- **MapGraph[N]**  
  An immutable graph that stores adjacency information as a `Map[N, Traversable[N]]`. Well-suited for static graphs of arbitrary node types.

- **IntMapGraph**  
  Like `MapGraph`, but specialized for integer nodes for performance.

- **MutableMapGraph[N]**  
  A mutable graph implementation, using a mutable map (`collection.mutable.Map`) from nodes to mutable adjacency lists. Supports direct mutation and efficient algorithms requiring graph rewrites.

- **GenericReverseGraph[N]**  
  A wrapper that presents the reversed view of any graph (swapping the direction of all edges on the fly).

### How to Choose

- Use `GenericGraph` and custom traits for maximum abstraction or advanced use cases.
- Use `MapGraph` or `IntMapGraph` for static graphs.
- Use `MutableMapGraph` when you need to build or alter the graph dynamically.

With these types and traits, you can define, extend, or modify graphs to suit most algorithmic needs, using Scala’s collections and type system for flexibility and safety.


## Mermaid diagrams

The library provides utilities for rendering graphs in [Mermaid](https://mermaid-js.github.io/) syntax for easy visualization in Markdown files, documentation, and compatible tools.

#### Simple graph diagram

You can generate a Mermaid representation of a `Graph` using the `Mermaid.render` method:

```scala
val g = Graph(1 -> Seq(2, 3), 2 -> Seq(3), 3 -> Seq(4), 4 -> Seq(2))
val mermaid =
  Mermaid.renderGraph(g, Mermaid.Direction.LeftToRight)

println(mermaid)
```

This prints:

```mermaid
graph LR
1
2
3
4
1-->2
1-->3
2-->3
3-->4
4-->2
```

#### Graph diagram with clases and edge types

You can further customize node styles and edge types using an extended version of `Mermaid.render`:

```scala
type Node = Int | String
val g = Graph[Node](
  1   -> Seq(2, 3),
  2   -> Seq(3),
  4   -> Seq("c"),
  "a" -> Seq(4),
  "b" -> Seq(1, "a"),
  "c" -> Seq("b", 2)
)
val mermaid = Mermaid.renderGraph(
  g,
  classDef = {
    case "int"    => "fill:#2058FF,stroke:#2058FF,color:#fff"
    case "string" => "fill:#FF00BF,stroke:#FF00BF,color:#fff"
  },
  nodeClass = {
    case n: Int    => "int"
    case n: String => "string"
  },
  edgeType = {
    case (_: Int, _: Int)       => "-->"
    case (_: String, _: String) => "==>"
    case (_: Int, _: String)    => "-->"
    case (_: String, _: Int)    => "-.->"
  },
  direction = Mermaid.Direction.TopToBottom
)

println(mermaid)
```

Example output:

```mermaid
graph TB
classDef int fill:#2058FF,stroke:#2058FF,color:#fff
classDef string fill:#FF00BF,stroke:#FF00BF,color:#fff
1:::int
a:::string
2:::int
b:::string
c:::string
4:::int
1-->2
1-->3
a-.->4
2-->3
b-.->1
b==>a
c==>b
c-.->2
4-->c
```

#### Simple state diagram

The `Mermaid` utility also supports rendering [state diagrams](https://mermaid.js.org/syntax/stateDiagram.html) in Mermaid syntax using its `renderStateDiagramV2` methods.

You can render a simple state diagram by providing a directed graph, starts and ends will be derived from graph's rootes and leaves:

```scala
val g = Graph[Int](
  1 -> Seq(3, 4),
  2 -> Seq(3),
  3 -> Seq(4),
  4 -> Seq(3, 5),
  5 -> Seq()
)
val mermaid = Mermaid.renderStateDiagramV2(g)
println(mermaid)
```

**Output:**

```mermaid
stateDiagram-v2
    direction TB
    [*] --> 1
    [*] --> 2
    1 --> 3
    1 --> 4
    2 --> 3
    3 --> 4
    4 --> 3
    4 --> 5
    5 --> [*]
```

#### State diagram with classes

You can render a state diagram with custom start/end nodes and style classes. For example:

```scala
val graph = Graph[Int](
  1 -> Seq(3, 4),
  2 -> Seq(3),
  3 -> Seq(4),
  4 -> Seq(3, 5),
  5 -> Seq()
)
val mermaid = Mermaid.renderStateDiagramV2(
  graph,
  starts = Seq(1, 2),
  ends = Seq(5),
  classDefs = Map(
    "foo" -> "fill:#2058FF,stroke:#2058FF,color:#fff",
    "bar" -> "fill:#FF00BF,stroke:#FF00BF,color:#fff"
  ),
  nodeClass = {
    case 1 | 2 | 5 => "foo"
    case 3 | 4     => "bar"
  },
  direction = Mermaid.Direction.LeftToRight
)
println(mermaid)
```

**Output:**

```mermaid
stateDiagram-v2
    direction LR
    classDef foo fill:#2058FF,stroke:#2058FF,color:#fff
    classDef bar fill:#FF00BF,stroke:#FF00BF,color:#fff
    [*] --> 1:::foo
    [*] --> 2:::foo
    1:::foo --> 3:::bar
    1:::foo --> 4:::bar
    2:::foo --> 3:::bar
    3:::bar --> 4:::bar
    4:::bar --> 3:::bar
    4:::bar --> 5:::foo
    5:::foo --> [*]
```


## Project content

```
├── .github
│   └── workflows
│       ├── pages.yaml
│       ├── release.yaml
│       └── test.yaml
│
├── .gitignore
├── .scalafmt.conf
├── Graph.scala
├── Graph.test.scala
├── Heap.scala
├── Heap.test.scala
├── IntTraversable.scala
├── LICENSE
├── Mermaid.scala
├── Mermaid.test.scala
├── project.scala
├── QuickSort.scala
├── README.md
├── test-resources
│   ├── dijkstraData.txt
│   ├── graph1.txt
│   ├── HashInt.txt
│   ├── inversions.txt
│   ├── Median.txt
│   ├── quicksort.txt
│   └── SCC.txt
│
├── test.sh
├── Traversable.scala
└── Traversable.test.scala
```

