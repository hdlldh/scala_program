package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- Arbitrary.arbitrary[A]
    m <- Gen.oneOf(Gen.const(empty), genHeap)
  } yield insert(v, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("nsert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    val m = findMin(h)
    m == n1 || m == n2
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("Insert an element into an empty heap, then delete the minimum, the resulting heap should be empty") = forAll { (n: A) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  property("Get a sorted sequence of elements when continually finding and deleting minima.") = forAll { (h: H) =>
    def isSorted(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h1 = deleteMin(h)
        isEmpty(h1) || m <= findMin(h1) && isSorted(h1)
      }
    }
    isSorted(h)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("Find a minimum of the melding of any two heaps should return a minimum of one or the other") = forAll { (h1: H, h2: H) =>
    val m = findMin(meld(h1, h2))
    m == findMin(h1) || m == findMin(h2)
  }

  // Two heaps should be equal if recursivly removing min elements result in same elements until empty"
  property("Two heaps should be equal if recursivly removing min elements result in same elements until empty") = forAll { (h1: H, h2: H) =>
    def isSame(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && isSame(deleteMin(h1), deleteMin(h2))
      }
    }
    isSame(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
