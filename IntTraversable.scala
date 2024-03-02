package org.encalmo.data

import scala.language.implicitConversions
import scala.util.boundary

import boundary.break

/** Abstraction of the collection of integers accessible mainly using the foreach method.
  */
trait IntTraversable extends Traversable[Int] {

  override def exists(p: Int => Boolean): Boolean =
    boundary {
      foreach(n => if (p(n)) then break(true))
      false
    }

  override def contains(item: Int): Boolean =
    boundary {
      foreach(n => if (n == item) then break(true))
      false
    }

  override def find(p: Int => Boolean): Option[Int] =
    boundary {
      foreach(n => if (p(n)) then break(Some(n)))
      None
    }

  override def size: Int = {
    var count: Int = 0
    foreach(_ => count = count + 1)
    count
  }

  override def isEmpty: Boolean =
    boundary {
      foreach(n => break(false))
      true
    }

  override def foldLeft[R](initial: R)(f: (R, Int) => R): R = {
    var result: R = initial
    foreach(n => result = f(result, n))
    result
  }

  override def take(count: Int): IntTraversable = {
    val self = this
    new IntTraversable {
      def foreach[U](f: Int => U): Unit =
        boundary {
          var c: Int = 0
          self.foreach { n =>
            if (c < count) then {
              f(n)
              c = c + 1
            } else break()
          }
        }
    }
  }

  override def drop(count: Int): IntTraversable = {
    val self = this
    new IntTraversable {
      def foreach[U](f: Int => U): Unit =
        boundary {
          var c: Int = 0
          self.foreach { n =>
            if (c >= count) then f(n)
            c = c + 1
          }
        }
    }
  }

  override def minBy[B](f: Int => B)(implicit ord: Ordering[B]): Int =
    boundary {
      foreach { n =>
        var lowest: B = f(n)
        var item: Int = n
        foreach { n =>
          val b = f(n)
          if (ord.lt(b, lowest)) then {
            lowest = b
            item = n
          }
        }
        break(item)
      }
      throw new UnsupportedOperationException("empty.minBy")
    }

  override def maxBy[B](f: Int => B)(implicit ord: Ordering[B]): Int =
    boundary {
      foreach { n =>
        var greatest: B = f(n)
        var item: Int = n
        foreach { n =>
          val b = f(n)
          if (ord.gt(b, greatest)) then {
            greatest = b
            item = n
          }
        }
        break(item)
      }
      throw new UnsupportedOperationException("empty.maxBy")
    }

  override def toSeq: Seq[Int] = Traversable.toSeq(this)

  override def toMutableSeq: collection.mutable.Seq[Int] = {
    val builder = collection.mutable.Seq.newBuilder[Int]
    foreach(n => builder.addOne(n))
    builder.result()
  }

  override def filter(p: Int => Boolean): IntTraversable = withFilter(p)

  override def withFilter(p: Int => Boolean): IntTraversable = {
    val self = this
    new IntTraversable {
      def foreach[U](f: Int => U): Unit =
        self.foreach(n => if (p(n)) f(n))
    }
  }

  override def filterNot(p: Int => Boolean): IntTraversable = {
    val self = this
    new IntTraversable {
      def foreach[U](f: Int => U): Unit =
        self.foreach(n => if (!p(n)) f(n))
    }
  }

  override def toString(): String = {
    val b = new StringBuilder("IntTraversable(")
    var isNext = false
    foreach { n =>
      if (isNext) then b.append(",") else isNext = true
      b.append(n.toString())
    }
    b.append(")")
    b.toString()
  }

}
