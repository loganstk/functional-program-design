package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.{collect, forAll, propBoolean, classify}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap :

  lazy val nonEmptyHeap: Gen[H] =
    for
      num <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    yield
      insert(num, heap)

  lazy val genHeap: Gen[H] = frequency((1, const(empty)), (3, nonEmptyHeap))

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    classify(isEmpty(h), "empty") {
      val m = if isEmpty(h) then 0 else findMin(h)
      findMin(insert(m, h)) == m }
  }

  property("findMin returns the smallest of two elements") = forAll { (a: Int, b: Int, h: H) =>
    classify(isEmpty(h), "empty") {
      isEmpty(h) ==> {
        val afterA = insert(a, h)
        val afterB = insert(b, afterA)
        findMin(afterB) == Math.min(a, b)
      }
    }
  }

  property("insert into empty then deleteMin results in an empty heap") = forAll { (x: Int, h: H) =>
    classify(isEmpty(h), "empty") {
      isEmpty(h) ==> {
        val singleton = insert(x, h)
        isEmpty(deleteMin(singleton))
      }
    }
  }

  property("idk wtf") = forAll { (x: Int, h: H) => (isEmpty(h)) ==> true }

  def heap2list(h: H): List[Int] = {
    def traverseRec(h: H, result: List[Int]): List[Int] = {
      if (isEmpty(h)) result.reverse
      else traverseRec(deleteMin(h), findMin(h) :: result)
    }
    traverseRec(h, Nil)
  }

  property("calls to findMin return elements in ordered manner") = forAll { (h: H) =>
    (!isEmpty(h)) ==> {
      def popOneAndCompare(prev: Int, heap: H): Boolean =
        if (isEmpty(heap)) true
        else {
          val next = findMin(heap)
          if (next >= prev) popOneAndCompare(next, deleteMin(heap)) else false
        }
      popOneAndCompare(Integer.MIN_VALUE, h)
    }
  }

  property("minimum of meld is one of two findMin") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val combined = meld(h1, h2)
      findMin(combined) == min1 || findMin(combined) == min2
    }
  }
