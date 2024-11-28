package org.encalmo.data

import scala.collection.IterableOps
import scala.collection.mutable.ArrayDeque
import scala.io.Source
import scala.language.implicitConversions
import scala.util.Random
import scala.util.boundary
import org.encalmo.data.Traversable

import boundary.break
import collection.mutable.{
  ArrayBuffer,
  Map => MutableMap,
  Seq => MutableSeq,
  HashMap,
  HashSet,
  Queue,
  Stack,
  Growable,
  Shrinkable
}
import scala.collection.immutable.ArraySeq

/** Base abstraction of the graph. */
trait Graph[N] {
  def nodes: Traversable[N]
  def adjacent: N => Traversable[N]
  def hasAdjacent(node: N): Boolean
  def edges: Traversable[(N, N)]
  def contains(node: N): Boolean
  def reverse: Graph[N]
  def nodesCount: Int
  def edgesCount: Long
}

/** Weighted graph add-on */
trait Weighted[N, V] {
  def weight: (N, N) => V
}

/** Mutable graph add-on */
trait Mutable[N] extends Growable[(N, N)] with Shrinkable[(N, N)] {
  def remove(node: N): this.type
  def transform(f: (N, ArrayBuffer[N]) => ArrayBuffer[N]): this.type
  def update(node: N, adjacent: ArrayBuffer[N]): this.type
}

/** Simple graph defined by abstract nodes collection and adjacent nodes function. */
trait GenericGraph[N] extends Graph[N] {
  self =>
  override def edges: Traversable[(N, N)] = new Traversable[(N, N)] {
    override def foreach[U](f: ((N, N)) => U): Unit = for (from <- nodes; to <- adjacent(from)) do f((from, to))
  }
  override def contains(node: N): Boolean = nodes.contains(node)
  override def reverse: Graph[N] = new GenericReverseGraph[N](self)
  override def nodesCount: Int = nodes.size
  override def edgesCount: Long = nodes.foldLeft(0L) { case (sum, node) =>
    sum + adjacent(node).size
  }
}

final class GenericReverseGraph[N](origin: Graph[N]) extends GenericGraph[N] {
  override def nodes: Traversable[N] = origin.nodes
  override val adjacent: N => Traversable[N] = node =>
    Traversable[N] { f =>
      for (n <- origin.nodes if (origin.adjacent(n).contains(node))) do f(n)
    }
  override def hasAdjacent(node: N): Boolean =
    origin.nodes.exists(n => adjacent(n).contains(node))

  override def edges: Traversable[(N, N)] = new Traversable[(N, N)] {
    def foreach[U](f: ((N, N)) => U) = {
      for (from <- origin.nodes; to <- origin.adjacent(from)) f((to, from))
    }
  }
  override val reverse = origin
}

class MapGraph[N](
    private val nodeMap: Map[N, Traversable[N]] = Map[N, Traversable[N]]()
) extends GenericGraph[N] {
  override val nodes: Traversable[N] = Traversable.of(nodeMap.keys)
  override val adjacent: N => Traversable[N] = nodeMap
  override def hasAdjacent(node: N): Boolean = nodeMap.get(node).exists(_.nonEmpty)
  override lazy val reverse: Graph[N] = Graph.hardCopyReversed[N](this)
  override def contains(node: N): Boolean = nodeMap.contains(node)

  override def toString(): String =
    s"MapGraph($nodeMap)"
}

final class IntMapGraph(
    private val nodeMap: Map[Int, Traversable[Int]] = Map[Int, Traversable[Int]]()
) extends MapGraph[Int] {
  override val nodes: Traversable[Int] = Traversable.of(nodeMap.keys)
  override val adjacent: Int => Traversable[Int] = nodeMap
  override inline def hasAdjacent(node: Int): Boolean = nodeMap.get(node).exists(_.nonEmpty)
  override lazy val reverse: Graph[Int] = Graph.hardCopyReversed[Int](this)
  override inline def contains(node: Int): Boolean = nodeMap.contains(node)

  override def toString(): String =
    s"IntMapGraph($nodeMap)"
}

object MutableMapGraph {
  inline def from[N](graph: Graph[N]): MutableMapGraph[N] =
    graph match {
      case x: MutableMapGraph[N] => x
      case _                     => Graph.hardCopy(graph)
    }
}

class MutableMapGraph[N](
    protected val nodeMap: MutableMap[N, ArrayBuffer[N]] = new HashMap[N, ArrayBuffer[N]]()
) extends GenericGraph[N]
    with Mutable[N] {

  final override inline def nodes: Traversable[N] = nodeMap.keys
  final override inline def nodesCount: Int = nodeMap.size
  final override inline def contains(node: N): Boolean = nodeMap.contains(node)

  override val adjacent: N => Traversable[N] = nodeMap.view.mapValues(v => Traversable.of(v))
  override def hasAdjacent(node: N): Boolean = nodeMap.get(node).exists(_.nonEmpty)
  override def reverse: Graph[N] = Graph.hardCopyReversed[N](this)

  final override def addOne(edge: (N, N)): this.type = {
    nodeMap.getOrElseUpdate(edge._1, { new ArrayBuffer[N]() }).addOne(edge._2)
    nodeMap.getOrElseUpdate(edge._2, { new ArrayBuffer[N]() })
    this
  }

  final def prependOne(edge: (N, N)): this.type = {
    nodeMap.getOrElseUpdate(edge._1, { new ArrayBuffer[N]() }).prepend(edge._2)
    nodeMap.getOrElseUpdate(edge._2, { new ArrayBuffer[N]() })
    this
  }

  final def prependOneIfNotExist(edge: (N, N)): this.type = {
    nodeMap.updateWith(edge._1) {
      case Some(existing) =>
        if (!existing.contains(edge._2)) then Some(existing.prepend(edge._2)) else Some(existing)
      case None => Some(ArrayBuffer(edge._2))
    }
    nodeMap.updateWith(edge._2)(_.orElse(Some(ArrayBuffer.empty)))
    this
  }

  final inline def addAll(xs: Traversable[(N, N)]): this.type = {
    xs.foreach(addOne)
    this
  }

  final override def subtractOne(edge: (N, N)): this.type = {
    for (adjacent <- nodeMap.get(edge._1)) {
      val a: ArrayBuffer[N] = adjacent
      a -= (edge._2)
    }
    this
  }

  final inline def subtractAll(xs: Traversable[(N, N)]): this.type = {
    xs.foreach(subtractOne)
    this
  }

  final override def remove(node: N): this.type = {
    nodeMap.subtractOne(node)
    this
  }

  final override def transform(f: (N, ArrayBuffer[N]) => ArrayBuffer[N]): this.type = {
    nodeMap.mapValuesInPlace(f)
    this
  }

  final override def update(node: N, adjacent: ArrayBuffer[N]): this.type = {
    nodeMap(node) = adjacent
    this
  }

  final def head: (N, ArrayBuffer[N]) = nodeMap.head

  final inline def addReverse(edge: (N, N)): this.type = this += (edge.swap)

  final inline def addReverse(edges: Traversable[(N, N)]): this.type = {
    for (edge <- edges) +=(edge.swap); this
  }

  final inline override def clear() = { nodeMap.clear() }

  final def merge(other: MutableMapGraph[N]): MutableMapGraph[N] =
    if (other.nodeMap.isEmpty) then this
    else if (this.nodeMap.isEmpty) then other
    else {
      val mergedNodeMap = new HashMap[N, ArrayBuffer[N]]()
      this.nodeMap.foreach(mergedNodeMap.addOne)
      other.nodeMap.foreach((node, added) =>
        mergedNodeMap.updateWith(node) {
          case Some(existing) =>
            if (added.isEmpty) then Some(existing)
            else if (existing.isEmpty) then Some(added)
            else {
              val adjacent = ArrayBuffer.from(existing)
              added.foreach(n => if (!adjacent.contains(n)) then adjacent.addOne(n))
              Some(adjacent)
            }
          case None => Some(added)
        }
      )
      MutableMapGraph(mergedNodeMap)
    }
}

/** Graph operations. */
object Graph {

  object GraphCycleFoundException extends Exception

  final class GenericGraphImpl[N](
      val nodes: Traversable[N],
      val adjacent: N => Traversable[N]
  ) extends GenericGraph[N] {

    override def hasAdjacent(node: N): Boolean = adjacent(node).nonEmpty
  }

  final class WeightedGraphImpl[N, V: Numeric](
      val nodes: Traversable[N],
      val adjacent: N => Traversable[N],
      val weight: (N, N) => V
  ) extends GenericGraph[N]
      with Weighted[N, V] {

    override def hasAdjacent(node: N): Boolean = adjacent(node).nonEmpty
  }

  def apply[N](): MutableMapGraph[N] = new MutableMapGraph[N]()

  inline def apply[N](map: Map[N, Traversable[N]]): MapGraph[N] =
    inline scala.compiletime.erasedValue[N] match {
      case _: Int =>
        new IntMapGraph(map.asInstanceOf[Map[Int, Traversable[Int]]])
          .asInstanceOf[MapGraph[N]]

      case _ =>
        new MapGraph(map)
    }

  inline def apply[N](mappings: (N, Traversable[N])*): MapGraph[N] =
    inline scala.compiletime.erasedValue[N] match {
      case _: Int =>
        new IntMapGraph(mappings.toMap.asInstanceOf[Map[Int, Traversable[Int]]])
          .asInstanceOf[MapGraph[N]]

      case _ =>
        new MapGraph(mappings.toMap)
    }

  inline def apply[N](nodes: Iterable[N], adjacent: N => Traversable[N]): Graph[N] =
    new GenericGraphImpl[N](nodes, adjacent)

  inline def apply[N](edges: Traversable[(N, N)]): MutableMapGraph[N] =
    new MutableMapGraph[N]().addAll(edges)

  inline def apply[N](edges: Iterator[(N, N)]): MutableMapGraph[N] =
    new MutableMapGraph[N]().addAll(edges)

  def apply[N, V: Numeric](
      mappings: (N, Iterable[(N, V)])*
  ): Graph[N] & Weighted[N, V] = {
    val nodeWeightMap = mappings.toMap map { case (k, v) => (k, v.toMap) }
    new WeightedGraphImpl[N, V](
      nodeWeightMap.keys,
      nodeWeightMap.view.mapValues { case m => m.keys },
      (t: N, h: N) => nodeWeightMap(t)(h)
    )
  }

  inline def hardCopy[N](graph: Graph[N]): MutableMapGraph[N] =
    new MutableMapGraph[N]().addAll(graph.edges)

  final class ReversedMutableMapGraph[N](_reverse: Graph[N]) extends MutableMapGraph[N] {
    override lazy val reverse: Graph[N] = _reverse
  }

  inline def hardCopyReversed[N](graph: Graph[N]): MutableMapGraph[N] = {
    new ReversedMutableMapGraph[N](graph).addReverse(graph.edges)
  }

  def readFromEdgeListFile(
      path: Source,
      reversed: Boolean = false
  ): Graph[Int] = {
    val edges: Iterator[(Int, Int)] = path
      .getLines()
      .map(line => {
        val i = line.indexOf(' ')
        val tail = line.substring(0, i).toInt
        val head = line.substring(i + 1).trim.toInt
        if (reversed) (head, tail) else (tail, head)
      })
    Graph(edges)
  }

  def readFromAdjacentListFile(path: Source): Graph[Int] = {
    def parseNodeAdjacentList(line: String): (Int, Seq[Int]) = {
      val tokens = line.split('\t')
      if (tokens.length == 0) return null
      val label: Int = Integer.parseInt(tokens(0))
      val adjacent: Seq[Int] = ArraySeq.unsafeWrapArray(tokens.drop(1).map(_.toInt))
      (label, adjacent)
    }
    val nodeMap = MutableMap[Int, Traversable[Int]]()
    for (line <- path.getLines() if !line.trim.isEmpty) {
      val (node, adjacent) = parseNodeAdjacentList(line)
      nodeMap(node) = adjacent
    }
    new GenericGraphImpl[Int](nodeMap.keys, nodeMap)
  }

  def readFromAdjacentWeightListFile(
      path: Source
  ): Graph[Int] & Weighted[Int, Int] = {

    def parseNodeWeightAdjacentList(line: String): (Int, Map[Int, Int]) = {
      val tokens = line.split('\t')
      if (tokens.length == 0) return null
      val label: Int = Integer.parseInt(tokens(0))
      val adjacent: Map[Int, Int] = (tokens.drop(1) map parseNodeWeight).toMap
      (label, adjacent)
    }

    def parseNodeWeight(token: String): (Int, Int) = {
      val nw = token.split(',') map (_.toInt); (nw(0), nw(1))
    }

    val nodeWeightMap = MutableMap[Int, Map[Int, Int]]()

    for (line <- path.getLines() if !line.trim.isEmpty) {
      val (node, list) = parseNodeWeightAdjacentList(line)
      nodeWeightMap(node) = list
    }

    new WeightedGraphImpl[Int, Int](
      nodeWeightMap.keys,
      nodeWeightMap.view.mapValues { case m => m.keys },
      (t: Int, h: Int) => nodeWeightMap(t)(h)
    )
  }

  trait DfsVisitor[N] {
    def start(node: N) = {}
    def before(node: N) = {}
    def edge(edge: (N, N)) = {}
    def after(node: N) = {}
  }

  extension [N](graph: Graph[N])

    /** Depth-first search of the whole graph */
    inline def dfs(visitor: DfsVisitor[N]): Unit =
      Graph.dfs(graph)(visitor, graph.nodes)

    /** Depth-first search of the whole graph in the given node's order */
    def dfs(
        visitor: DfsVisitor[N],
        nodes: Traversable[N]
    ): Unit = {
      val explored = new HashSet[N]()
      for (node <- nodes) do {
        if (!(explored.contains(node))) {
          visitor.start(node)
          Graph.dfsi(graph)(node, visitor, explored)
        }
      }
    }

    /** Depth-first search (recursive) of the graph starting at given node */
    def dfs(
        node: N,
        visitor: DfsVisitor[N],
        explored: HashSet[N] = HashSet[N]()
    ): Unit = {
      if (!(explored.contains(node))) {
        explored.add(node)
        visitor.before(node)
        for (next <- graph.adjacent(node) if (!explored.contains(next))) do {
          visitor.edge((node, next))
          Graph.dfs(graph)(next, visitor, explored)
        }
        visitor.after(node)
      }
    }

    /** Depth-first search (iterative) of the graph starting at given node */
    def dfsi(
        source: N,
        visitor: DfsVisitor[N],
        explored: HashSet[N] = HashSet[N]()
    ): Unit = {
      val stack = new Stack[N]()
      explored add source
      stack.push(source)
      visitor.before(source)
      while (!stack.isEmpty) {
        val node = stack.top
        graph.adjacent(node).find(n => !explored.contains(n)) match {
          case Some(next) => {
            explored add next
            stack.push(next)
            visitor.edge((node, next))
            visitor.before(next)
          }
          case None => {
            stack.pop()
            visitor.after(node)
          }
        }
      }
    }

    /** Breath-first search of the whole graph */
    def bfs(visitor: N => Unit): Unit = {
      val explored = HashSet[N]()
      for (node <- graph.nodes) {
        if (!(explored.contains(node))) {
          Graph.bfs(graph)(node, visitor, explored)
        }
      }
    }

    /** Breath-first search of the graph starting at given node */
    def bfs(
        node: N,
        visitor: N => Unit,
        explored: HashSet[N] = HashSet[N]()
    ): Unit = {
      val queue = new Queue[N]()
      queue.enqueue(node)
      while (!queue.isEmpty) {
        val n = queue.dequeue
        if (!(explored.contains(n))) {
          explored.add(n)
          visitor(n)
          for (next <- graph.adjacent(n)) queue.enqueue(next)
        }
      }
    }

    def findCycles: Vector[N] = {
      var cycles: Vector[N] = Vector.empty[N]
      val marks = new HashMap[N, Char]().withDefaultValue('0')
      for (node <- graph.nodes if (marks(node) == '0')) {
        cycles = cycles ++ Graph.findCycles(graph)(node, marks)
      }
      cycles
    }

    def findCycles(
        node: N,
        marks: MutableMap[N, Char] = new HashMap[N, Char]().withDefaultValue('0')
    ): Vector[N] = {
      var cycles: Vector[N] = Vector.empty[N]
      if (marks(node) == 'x') cycles = cycles :+ node
      else if (marks(node) == '0') {
        marks(node) = 'x'
        graph.adjacent(node) foreach { next =>
          cycles = cycles ++ Graph.findCycles(graph)(next, marks)
        }
        marks(node) = '1'
      }
      cycles
    }

    def hasCycles: Boolean = {
      val marks = new HashMap[N, Char]().withDefaultValue('0')
      def checkCycles(node: N): Unit = {
        if (marks(node) == 'x') throw GraphCycleFoundException
        else if (marks(node) == '0') {
          marks(node) = 'x'
          graph.adjacent(node).foreach(checkCycles)
          marks(node) = '1'
        }
      }
      try {
        for (node <- graph.nodes if (marks(node) == '0')) checkCycles(node)
        false
      } catch {
        case GraphCycleFoundException => true
      }
    }

    def sortTopologically: List[N] = {
      var counter = graph.nodesCount
      var priorities: List[N] = Nil
      val observer = new DfsVisitor[N] {
        override inline def after(node: N) = {
          priorities = node :: priorities
          counter = counter - 1
        }
      }
      Graph.dfs(graph)(observer)
      priorities
    }

    /* Kosaraju's 2-dfs pass algorithm finds strongly connected components */
    def findStronglyConnectedComponents: Traversable[Traversable[N]] = {
      val reversed: Graph[N] = graph.reverse
      val nodes = graph.nodes.toMutableSeq
      // first dfs pass
      val times = new HashMap[N, Int]()
      Graph.dfs(reversed)(
        new DfsVisitor[N] {
          var time: Int = 0
          override def after(node: N) = {
            time = time + 1
            times(node) = time
          }
        },
        nodes
      )
      // sorting nodes by reversed entry time
      val ordering = new Ordering[N] {
        def compare(x: N, y: N): Int = times(y) - times(x)
      }
      QuickSort.sort(nodes)(using ordering)
      // second dfs pass
      val leaders = new HashMap[N, N]()
      Graph.dfs(graph)(
        new DfsVisitor[N] {
          var leader: Option[N] = None
          override def start(node: N) = {
            leader = Some(node)
          }
          override def before(node: N) = {
            leaders(node) = leader.get
          }
        },
        nodes
      )
      // result computing
      graph.nodes.sortedValuesOfGroupBy(leaders)(c => -c.size)
    }

    def mergeNodes(
        mergedNode: N,
        removedNode: N
    ): MutableMapGraph[N] = {
      val g = MutableMapGraph.from(graph)
      // merge two adjacent lists, remove self-loops
      val removedAdjacent = g.adjacent(removedNode)
      val mergedAdjacent = g.adjacent(mergedNode)
      val newAdjacent =
        new ArrayBuffer[N](removedAdjacent.size + mergedAdjacent.size)
      for (node <- mergedAdjacent) {
        if (node != removedNode) newAdjacent.addOne(node)
      }
      for (node <- removedAdjacent) {
        if (node != mergedNode) newAdjacent.addOne(node)
      }
      g.remove(removedNode) // remove node
      g(mergedNode) = newAdjacent // set new adjacent for mergedNode
      g.transform { (_, adjacent) =>
        {
          if (adjacent.contains(removedNode)) {
            adjacent map {
              case n if n == removedNode => mergedNode
              case n                     => n
            }
          } else {
            adjacent
          }
        }
      }
      g
    }

    def randomCutCount: Int = {

      def randomizedQueue[N](seq: Traversable[N]): Queue[N] = {
        val queue: Queue[N] = Queue()
        seq.foreach { n =>
          if (Random.nextBoolean())
          then queue.append(n)
          else queue.prepend(n)
        }
        queue
      }

      val g = MutableMapGraph.from(graph)
      val nodesQueue = randomizedQueue(g.nodes)
      while (g.nodesCount > 2) {
        val node1 = nodesQueue.dequeue
        val adjacent = g.adjacent(node1)
        if (adjacent.size > 0) {
          val j = (Math.random() * adjacent.size).asInstanceOf[Int]
          val node2 = adjacent.get(j)
          Graph.mergeNodes(g)(node2, node1)
        }
      }
      val (_, adjacent) = g.head
      adjacent.size
    }

    /** Merges two graphs without duplicating existing nodes */
    def merge(graph2: Graph[N]): Graph[N] =
      MutableMapGraph.from(graph).merge(MutableMapGraph.from(graph2))

    def leavesOf: Traversable[N] =
      graph.nodes.filterNot(n => graph.hasAdjacent(n))

    def rootsOf: Traversable[N] =
      val reversed = graph.reverse
      reversed.nodes.filterNot(n => reversed.hasAdjacent(n))

    /** Returns a new graph containing only all the transitive successors of the given node. */
    def successorsOf(node: N): Graph[N] =
      val result: MutableMapGraph[N] = MutableMapGraph()
      val queue = new Queue[N]()
      queue.enqueue(node)
      while (!queue.isEmpty) {
        val n = queue.dequeue
        if (!(result.contains(n))) {
          val adjacent = graph.adjacent(n)
          result.update(n, adjacent.toArrayBuffer)
          for (next <- adjacent) queue.enqueue(next)
        }
      }
      result

    /** Returns a new graph containing only all the transitive successors of the given nodes. */
    def successorsOf(nodes: N*): Graph[N] =
      val result: MutableMapGraph[N] = MutableMapGraph()
      val queue = new Queue[N]()
      queue.enqueueAll(nodes)
      while (!queue.isEmpty) {
        val n = queue.dequeue
        if (!(result.contains(n))) {
          val adjacent = graph.adjacent(n)
          result.update(n, adjacent.toArrayBuffer)
          for (next <- adjacent) queue.enqueue(next)
        }
      }
      result

    /** Returns a new graph containing only all the transitive predecessors of the given node. */
    def predecessorsOf(node: N): Graph[N] =
      val reversed = graph.reverse
      val result: MutableMapGraph[N] = MutableMapGraph()
      val explored = HashSet[N]()
      val queue = new Queue[N]()
      queue.enqueue(node)
      while (!queue.isEmpty) {
        val n = queue.dequeue
        if (!(explored.contains(n))) {
          explored.add(n)
          val adjacent = reversed.adjacent(n)
          for (next <- adjacent) {
            result.prependOne((next, n))
            queue.enqueue(next)
          }
        }
      }
      result

    /** Returns a new graph containing only all the transitive predecessors of the given nodes. */
    def predecessorsOf(nodes: N*): Graph[N] =
      val reversed = graph.reverse
      val result: MutableMapGraph[N] = MutableMapGraph()
      val explored = HashSet[N]()
      val queue = new Queue[N]()
      queue.enqueueAll(nodes)
      while (!queue.isEmpty) {
        val n = queue.dequeue
        if (!(explored.contains(n))) {
          explored.add(n)
          val adjacent = reversed.adjacent(n)
          for (next <- adjacent) {
            result.prependOne((next, n))
            queue.enqueue(next)
          }
        }
      }
      result

    /** Returns a new graph containing only all the transitive predecessors and successors of the given node. */
    def predecessorsAndSuccessorsOf(node: N): Graph[N] =
      val result: MutableMapGraph[N] = MutableMapGraph()
      val queue = new Queue[N]()
      queue.enqueue(node)
      while (!queue.isEmpty) {
        val n = queue.dequeue
        if (!(result.contains(n))) {
          val adjacent = graph.adjacent(n)
          result.update(n, adjacent.toArrayBuffer)
          for (next <- adjacent) queue.enqueue(next)
        }
      }
      lazy val reversed = graph.reverse
      val explored = result.nodes.toHashSet - node
      queue.enqueue(node)
      while (!queue.isEmpty) {
        val n = queue.dequeue
        if (!(explored.contains(n))) {
          explored.add(n)
          val adjacent = reversed.adjacent(n)
          for (next <- adjacent) {
            result.prependOneIfNotExist((next, n))
            queue.enqueue(next)
          }
        }
      }
      result

    /** Returns a new graph containing only all the transitive predecessors and successors of the given nodes. */
    def predecessorsAndSuccessorsOf(nodes: N*): Graph[N] =
      val result: MutableMapGraph[N] = MutableMapGraph()
      val queue = new Queue[N]()
      queue.enqueueAll(nodes)
      while (!queue.isEmpty) {
        val n = queue.dequeue
        if (!(result.contains(n))) {
          val adjacent = graph.adjacent(n)
          result.update(n, adjacent.toArrayBuffer)
          for (next <- adjacent) queue.enqueue(next)
        }
      }
      lazy val reversed = graph.reverse
      val explored = result.nodes.toHashSet -- nodes
      queue.enqueueAll(nodes)
      while (!queue.isEmpty) {
        val n = queue.dequeue
        if (!(explored.contains(n))) {
          explored.add(n)
          val adjacent = reversed.adjacent(n)
          for (next <- adjacent) {
            result.prependOneIfNotExist((next, n))
            queue.enqueue(next)
          }
        }
      }
      result

  extension [N, V: Numeric](graph: Graph[N] & Weighted[N, V])

    /** Dijkstra algorithm finds shortest path in directed graph */
    inline def findShortestPath(
        from: N,
        to: N
    ): (V, List[(N, N)]) = {
      graph.findShortestPath(from, to, graph.weight)
    }

    /** Dijkstra algorithm finds the shortest path in a directed graph */
    def findShortestPath(
        from: N,
        to: N,
        weight: (N, N) => V
    ): (V, List[(N, N)]) = {
      val num: Numeric[V] = implicitly[Numeric[V]]
      if (from == to || graph.adjacent(from).isEmpty) return (num.zero, Nil)
      val nodesCount = graph.nodesCount
      val explored = new HashSet[N]()
      val distance = new HashMap[N, V]()
      val backtrace = new MutableMapGraph[N]()
      implicit val ordering = new Ordering[(N, N, V)] {
        def compare(x: (N, N, V), y: (N, N, V)): Int = {
          num.toInt(
            num.minus(
              num.plus(distance(x._1), x._3),
              num.plus(distance(y._1), y._3)
            )
          )
        }
      }
      val outgoingEdges = new MinHeap[(N, N, V)](Math.min(graph.nodesCount, 1024))
      var head = from
      explored add from
      distance(from) = num.zero
      var nextEdges = graph.adjacent(from).filterNot(explored).map(node => (from, node, weight(from, node)))
      outgoingEdges.insert(nextEdges)
      while {
        for ((t, h, w) <- outgoingEdges.extract) {
          explored.add(h)
          distance(h) = num.plus(distance(t), w)
          backtrace += ((h, t))
          outgoingEdges.remove { case (_, node, _) => node == h }
          nextEdges = graph.adjacent(h).filterNot(explored).map(node => (h, node, weight(h, node)))
          outgoingEdges.insert(nextEdges)
          head = h
        }
        head != to && !outgoingEdges.isEmpty && explored.size != nodesCount
      } do ()
      // compute resulting path
      var path: List[(N, N)] = Nil
      if (head == to) {

        var next = to
        while {
          val node = next
          next = backtrace.adjacent(node).minBy(n => distance(n))
          val segment = (next, node)
          path = segment :: path
          next != from
        } do ()
      }
      (distance(to), path)
    }

    /** Dijkstra algorithm finds all shortest paths starting at given node in directed graph
      */
    inline def findShortestPaths(
        from: N
    ): scala.collection.Map[N, V] = {
      graph.findShortestPaths(from, graph.weight)
    }

    /** Dijkstra algorithm finds all shortest paths starting at given node in directed graph
      */
    def findShortestPaths(
        from: N,
        weight: (N, N) => V
    ): scala.collection.Map[N, V] = {
      val num: Numeric[V] = implicitly[Numeric[V]]
      if (graph.adjacent(from).isEmpty) return Map.empty
      val nodesCount = graph.nodesCount
      val explored = new HashSet[N]()
      val distance = new HashMap[N, V]()
      implicit val ordering = new Ordering[(N, N, V)] {
        def compare(x: (N, N, V), y: (N, N, V)): Int = {
          num.toInt(
            num.minus(
              num.plus(distance(x._1), x._3),
              num.plus(distance(y._1), y._3)
            )
          )
        }
      }
      val outgoingEdges = new MinHeap[(N, N, V)](Math.min(graph.nodesCount, 1024))
      var head = from
      explored add from
      distance(from) = num.zero
      var nextEdges = graph.adjacent(from).filterNot(explored).map(node => (from, node, weight(from, node)))
      outgoingEdges.insert(nextEdges)
      while {
        for ((t, h, w) <- outgoingEdges.extract) {
          explored add h
          distance(h) = num.plus(distance(t), w)
          outgoingEdges.remove { case (_, node, _) => node == h }
          nextEdges = graph.adjacent(h).filterNot(explored).map(node => (h, node, weight(h, node)))
          outgoingEdges.insert(nextEdges)
          head = h
        }
        !outgoingEdges.isEmpty && explored.size != nodesCount
      } do ()
      distance
    }
}
