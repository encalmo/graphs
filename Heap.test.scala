package org.encalmo.data

import scala.io.Source
import scala.language.implicitConversions

class HeapSpec extends munit.FunSuite {

  trait HeapFixture {

    val minHeap = new MinHeap[Int]
    minHeap.insert(1221)
    minHeap.insert(1101)
    minHeap.insert(100)
    minHeap.insert(2)
    minHeap.insert(3)
    minHeap.insert(999)

    val maxHeap = new MaxHeap[Int]
    maxHeap.insert(1221)
    maxHeap.insert(1101)
    maxHeap.insert(100)
    maxHeap.insert(2)
    maxHeap.insert(3)
    maxHeap.insert(999)
  }

  test("should calculate left, right and parent element index")(new HeapFixture {
    assert(minHeap.left(1) == 2, s"left of 1 should be 2")
    assert(minHeap.right(1) == 3, s"right of 1 should be 3")
    assert(minHeap.left(2) == 4, s"left of 2 should be 4")
    assert(minHeap.right(2) == 5, s"right of 2 should be 5")
    assert(minHeap.left(3) == 6, s"left of 3 should be 6")
    assert(minHeap.right(3) == 7, s"right of 3 should be 7")
    assert(minHeap.parent(2) == 1, s"parent of 2 should be 1")
    assert(minHeap.parent(3) == 1, s"parent of 3 should be 1")
    assert(minHeap.parent(6) == 3, s"parent of 6 should be 3")
    assert(minHeap.parent(7) == 3, s"parent of 7 should be 3")
  })

  test("should insert element while maintaining min-heap property")(new HeapFixture {
    assert(minHeap.head == 2, s"heap head should be 2 but is ${minHeap.head}")
  })

  test("should insert element while maintaining max-heap property")(new HeapFixture {
    assert(maxHeap.head == 1221, s"heap head should be 2 but is ${maxHeap.head}")
  })

  test("should extract head while maintaining heap property")(new HeapFixture {
    val elem = minHeap.extract.get
    assert(elem == 2, s"extracted should be 2 but is ${elem}")
    assert(minHeap.head == 3, s"heap head should be 3 but is ${minHeap.head}")
    val elem2 = minHeap.extract.get
    assert(elem2 == 3, s"extracted should be 3 but is ${elem2}")
    assert(minHeap.head == 100, s"heap head should be 100 but is ${minHeap.head}")
  })

  test("should remove element while maintaining heap property")(new HeapFixture {
    minHeap.remove(3)
    minHeap.remove(999)
    assert(minHeap.head == 2, s"heap head should be 2 but is ${minHeap.head}")
    assert(minHeap.size == 4, s"heap size should be 4 but is ${minHeap.size}")
    minHeap.extract
    assert(minHeap.head == 100, s"heap head should be 100 but is ${minHeap.head}")
    minHeap.extract
    assert(minHeap.head == 1101, s"heap head should be 1101 but is ${minHeap.head}")
  })

  test("should remove elements by predicate")(new HeapFixture {
    minHeap.remove(_ > 100)
    assert(minHeap.head == 2, s"heap head should be 2 but is ${minHeap.head}")
  })

  test("should remove elements by predicate")(new HeapFixture {
    minHeap.remove(_ <= 100)
    assert(minHeap.head == 999, s"heap head should be 999 but is ${minHeap.head}")
  })

  test("should compute sum of moving medians") {
    val path = Source.fromResource("Median.txt")
    var sum = 0
    val minheap = new MinHeap[Int]()
    val maxheap = new MaxHeap[Int]()
    for (line <- path.getLines())
      {
        val num = line.trim.toInt
        if (maxheap.isEmpty) {
          maxheap.insert(num)
        } else {
          if (num <= maxheap.head) {
            maxheap.insert(num)
          } else {
            minheap.insert(num)
          }
        }
        if (maxheap.size > minheap.size + 1) {
          maxheap.extract.map(minheap.insert)
        } else if (minheap.size > maxheap.size) {
          minheap.extract.map(maxheap.insert)
        }
        val median = maxheap.head
        sum = sum + median
        if (sum >= 10000) sum = sum % 10000

      }
      assert(sum == 1213)
  }

}
