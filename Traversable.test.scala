package org.encalmo.data

import scala.io.Source
import scala.collection.mutable.Buffer

class TraversableSpec extends munit.FunSuite {

  def chars(n: Int) = new Traversable[Char] {
    override def foreach[U](f: Char => U): Unit = (0 until n).foreach(i => f((97 + i).toChar))
  }

  test("take") {
    assertEquals(chars(100).take(10).toString(), "Traversable(a,b,c,d,e,f,g,h,i,j)")
  }

  test("map") {
    assertEquals(chars(4).map(n => n * 2).toString(), "Traversable(194,196,198,200)")
  }

  test("toSeq") {
    assertEquals(chars(4).toSeq, Seq('a', 'b', 'c', 'd'))
  }

  def ints(n: Int) = new IntTraversable {
    override def foreach[U](f: Int => U): Unit = (0 until n).foreach(f)
  }

  test("take") {
    assertEquals(ints(100).take(10).toString(), "IntTraversable(0,1,2,3,4,5,6,7,8,9)")
  }

  test("map") {
    assertEquals(ints(4).map(n => n * 2).toString(), "Traversable(0,2,4,6)")
  }

  test("toSeq") {
    assertEquals(ints(4).toSeq, Seq(0, 1, 2, 3))
  }

  test("toMutableSeq") {
    assertEquals(ints(4).toMutableSeq, collection.mutable.Seq(0, 1, 2, 3))
    val buffer = Buffer("a", "b", "c")
    assert(Traversable.of(buffer).toMutableSeq eq buffer)
    val set = collection.mutable.Set("a", "b", "c")
    val t = Traversable.of(set).toMutableSeq
    assert(t ne set)
  }

  test("Traversable.of") {
    assertEquals(Traversable.of(1 to 10).toString(), "Range 1 to 10")
    assertEquals(Traversable.of(Seq("a", "b", "c")).toString(), "List(a, b, c)")
    assertEquals(Traversable.of(Set("a", "b", "c")).toString(), "Set(a, b, c)")
    assertEquals(Traversable.of(Buffer("a", "b", "c")).toString(), "ArrayBuffer(a, b, c)")
  }

}
