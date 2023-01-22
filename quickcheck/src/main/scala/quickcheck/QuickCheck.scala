package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.{collect, forAll, propBoolean, classify}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap :

  lazy val nonEmptyHeap: Gen[H] =
    for
      num <- arbitrary[Int]
      heap <- frequency((1, const(empty)), (4, nonEmptyHeap))
    yield
      insert(num, heap)

  lazy val genHeap: Gen[H] = oneOf(const(empty), nonEmptyHeap)

  given Arbitrary[H] = Arbitrary(genHeap)
  property("gen1") = forAll { (h: H) =>
    classify(isEmpty(h), "empty") {
      val m = if isEmpty(h) then 0 else findMin(h)
      findMin(insert(m, h)) == m }
  }

  property("findMin returns the smallest of two elements") = forAll { (a: Int, b: Int, h: H) =>
    isEmpty(h) ==> {
      val afterA = insert(a, h)
      val afterB = insert(b, afterA)
      findMin(afterB) == Math.min(a, b)
    }
  }

  property("insert into empty then deleteMin results in an empty heap") = forAll { (x: Int, h: H) =>
    isEmpty(h) ==> {
      val singleton = insert(x, h)
      isEmpty(deleteMin(singleton))
    }
  }

  property("successive deleteMin should not skip elements") = forAll { (a: Int, b: Int, c: Int) =>
    val max = List(a, b, c).max
    val afterInsert = insert(c, insert(b, insert(a, empty)))
    val heap = deleteMin(deleteMin(afterInsert))
    findMin(heap) == max
  }

  def heap2list(h: H): List[Int] = {
    def traverseRec(h: H, result: List[Int]): List[Int] = {
      if (isEmpty(h)) result.reverse
      else traverseRec(deleteMin(h), findMin(h) :: result)
    }

    traverseRec(h, Nil)
  }

  property("calls to findMin return elements in ordered manner") = forAll(nonEmptyHeap) { (h: H) =>
    def popOneAndCompare(prev: Int, heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val curr = findMin(heap)
        if (curr >= prev) popOneAndCompare(curr, deleteMin(heap)) else false
      }
    }
    popOneAndCompare(Integer.MIN_VALUE, h)
  }

  property("minimum of meld is one of two findMin") = forAll(nonEmptyHeap, nonEmptyHeap) { (h1: H, h2: H) =>
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val combined = meld(h1, h2)
      findMin(combined) == min1 || findMin(combined) == min2
  }
