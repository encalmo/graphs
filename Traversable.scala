package org.encalmo.data

import scala.language.implicitConversions
import scala.util.boundary

import boundary.break
import scala.reflect.TypeTest

/** Abstraction of the collection of items accessible mainly using the {foreach} method.
  */
trait Traversable[N] {

  def foreach[U](f: N => U): Unit

  def exists(p: N => Boolean): Boolean =
    boundary {
      foreach(n => if (p(n)) then break(true))
      false
    }

  def contains(item: N): Boolean =
    boundary {
      foreach(n => if (n == item) then break(true))
      false
    }

  def find(p: N => Boolean): Option[N] =
    boundary {
      foreach(n => if (p(n)) then break(Some(n)))
      None
    }

  def size: Int = {
    var count: Int = 0
    foreach(_ => count = count + 1)
    count
  }

  def isEmpty: Boolean =
    boundary {
      foreach(n => break(false))
      true
    }

  def foldLeft[R](initial: R)(f: (R, N) => R): R = {
    var result: R = initial
    foreach(n => result = f(result, n))
    result
  }

  def take(count: Int): Traversable[N] = {
    val self = this
    Traversable { f =>
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

  def drop(count: Int): Traversable[N] = {
    val self = this
    Traversable { f =>
      boundary {
        var c: Int = 0
        self.foreach { n =>
          if (c >= count) then f(n)
          c = c + 1
        }
      }
    }
  }

  def minBy[B](f: N => B)(implicit ord: Ordering[B]): N =
    boundary {
      foreach { n =>
        var lowest: B = f(n)
        var item: N = n
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

  def maxBy[B](f: N => B)(implicit ord: Ordering[B]): N =
    boundary {
      foreach { n =>
        var greatest: B = f(n)
        var item: N = n
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

  inline def sameElements[B >: N](that: IterableOnce[B]): Boolean =
    boundary {
      val iter = that.iterator
      foreach { n =>
        if (!iter.hasNext || n != iter.next()) break(false)
      }
      !iter.hasNext
    }

  def toSeq: Seq[N] = Traversable.toSeq(this)

  def toMutableSeq: collection.mutable.Seq[N] = {
    val builder = collection.mutable.Seq.newBuilder[N]
    foreach(n => builder.addOne(n))
    builder.result()
  }

  def sortedValuesOfGroupBy[K](f: N => K)(s: Iterable[N] => Int): Traversable[Traversable[N]] = {
    val m = collection.mutable.Map.empty[K, collection.mutable.Buffer[N]]
    foreach(n => m.updateWith(f(n))(x => x.map(_.append(n)).orElse(Some(collection.mutable.Buffer(n)))))
    val values = m.values.toSeq.sortBy(s).map(Traversable.of)
    Traversable[Traversable[N]] { f =>
      values.foreach(v => f(v))
    }
  }

  def map[M](m: N => M): Traversable[M] = {
    val self = this
    Traversable { f =>
      self.foreach(n => f(m(n)))
    }
  }

  def filter(p: N => Boolean): Traversable[N] = withFilter(p)

  def withFilter(p: N => Boolean): Traversable[N] = {
    val self = this
    Traversable { f =>
      self.foreach(n => if (p(n)) f(n))
    }
  }

  def filterNot(p: N => Boolean): Traversable[N] = {
    val self = this
    Traversable { f =>
      self.foreach(n => if (!p(n)) f(n))
    }
  }

  override def toString(): String = {
    val b = new StringBuilder("Traversable(")
    var isNext = false
    foreach { n =>
      if (isNext) then b.append(",") else isNext = true
      b.append(n.toString())
    }
    b.append(")")
    b.toString()
  }

  def get(index: Int): N =
    boundary {
      if (index < 0) throw new IndexOutOfBoundsException()
      var i = 0
      foreach { n =>
        if (i == index) break(n)
        i = i + 1
      }
      throw new IndexOutOfBoundsException()
    }

}

object Traversable {

  inline def apply[N](inline body: (N => ?) => Unit): Traversable[N] =
    inline scala.compiletime.erasedValue[N] match {
      case _: Int =>
        (new IntTraversable {
          override def foreach[U](f: Int => U): Unit = body(f.asInstanceOf[Function[N, ?]])
        }).asInstanceOf[Traversable[N]]
      case _ =>
        new Traversable[N] {
          override def foreach[U](f: N => U): Unit = body(f)
        }
    }

  given fromIterable[N]: Conversion[Iterable[N], Traversable[N]] with
    inline def apply(x: Iterable[N]): Traversable[N] =
      inline scala.compiletime.erasedValue[N] match {
        case _: Int =>
          (new IntTraversable {
            override def foreach[U](f: Int => U): Unit = x.foreach(f.asInstanceOf[Function[N, ?]])
            override def exists(p: N => Boolean): Boolean = x.exists(p)
            override def find(p: N => Boolean): Option[N] = x.find(p)
            override def foldLeft[R](initial: R)(f: (R, N) => R): R = x.foldLeft(initial)(f)
            override def size: Int = x.size
            override def isEmpty: Boolean = x.isEmpty
            override def toSeq: collection.immutable.Seq[Int] = x.toSeq.asInstanceOf[Seq[Int]]
            override def toMutableSeq: collection.mutable.Seq[Int] =
              if (x.isInstanceOf[collection.mutable.Seq[?]])
              then x.asInstanceOf[collection.mutable.Seq[Int]]
              else super.toMutableSeq.asInstanceOf[collection.mutable.Seq[Int]]
            override def toString(): String = x.toString
            override def minBy[B](f: Int => B)(implicit ord: Ordering[B]): Int =
              x.minBy(f.asInstanceOf[Function[N, B]]).asInstanceOf[Int]
            override def maxBy[B](f: Int => B)(implicit ord: Ordering[B]): Int =
              x.maxBy(f.asInstanceOf[Function[N, B]]).asInstanceOf[Int]
          }).asInstanceOf[Traversable[N]]
        case _ =>
          new Traversable[N] {
            override def foreach[U](f: N => U): Unit = x.foreach(f)
            override def exists(p: N => Boolean): Boolean = x.exists(p)
            override def find(p: N => Boolean): Option[N] = x.find(p)
            override def foldLeft[R](initial: R)(f: (R, N) => R): R = x.foldLeft(initial)(f)
            override def size: Int = x.size
            override def isEmpty: Boolean = x.isEmpty
            override def toSeq: collection.immutable.Seq[N] = x.toSeq
            override def toMutableSeq: collection.mutable.Seq[N] =
              if (x.isInstanceOf[collection.mutable.Seq[?]])
              then x.asInstanceOf[collection.mutable.Seq[N]]
              else super.toMutableSeq
            override def toString(): String = x.toString
            override def minBy[B](f: N => B)(implicit ord: Ordering[B]): N = x.minBy(f)
            override def maxBy[B](f: N => B)(implicit ord: Ordering[B]): N = x.maxBy(f)
          }
      }

  given fromSet[N]: Conversion[Set[N], Traversable[N]] with
    def apply(x: Set[N]): Traversable[N] =
      new Traversable[N] {
        inline override def foreach[U](f: N => U): Unit = x.foreach(f)
        inline override def exists(p: N => Boolean): Boolean = x.exists(p)
        inline override def contains(item: N): Boolean = x.contains(item)
        inline override def find(p: N => Boolean): Option[N] = x.find(p)
        inline override def foldLeft[R](initial: R)(f: (R, N) => R): R = x.foldLeft(initial)(f)
        inline override def size: Int = x.size
        inline override def isEmpty: Boolean = x.isEmpty
        inline override def toSeq: collection.immutable.Seq[N] = x.toSeq
        inline override def toMutableSeq: collection.mutable.Seq[N] = x.toMutableSeq
        inline override def toString(): String = x.toString
        inline override def minBy[B](f: N => B)(implicit ord: Ordering[B]): N = x.minBy(f)
        inline override def maxBy[B](f: N => B)(implicit ord: Ordering[B]): N = x.maxBy(f)
      }

  def toIterable[N](x: Traversable[N]): Iterable[N] =
    new Iterable[N] {
      override inline def iterator: Iterator[N] = new Iterator[N] {

        var index = 0
        var nextItem: Option[Option[N]] = None

        private def maybe(index: Int): Option[N] =
          boundary {
            var i = 0
            x.foreach { n =>
              if (i == index) break(Some(n))
              i = i + 1
            }
            None
          }

        override def hasNext: Boolean =
          nextItem match {
            case None =>
              nextItem = Some(maybe(index))
              nextItem.map(_.isDefined).getOrElse(false)
            case Some(item) =>
              item.isDefined
          }

        override def next(): N =
          nextItem match {
            case None =>
              val item = maybe(index)
              index = index + 1
              nextItem = None
              item.get
            case Some(item) =>
              index = index + 1
              nextItem = None
              item.get
          }

      }
    }

  inline def of[N](x: Iterable[N]): Traversable[N] = fromIterable(x)
  inline def of[N](x: Set[N]): Traversable[N] = fromSet(x)
  inline def toSeq[N](x: Traversable[N]): Seq[N] = toIterable(x).toSeq

}
