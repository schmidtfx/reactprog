package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == (a min b)
  }

  property("del1") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("del2") = forAll { (h: H) =>
    val l = delMinAndCheck(h)
    (l, l.tail).zipped.forall(_ <= _)
  }

  property("comb1") = forAll { (h: H, other: H) =>
    val newH = meld(h, other)
    findMin(newH) == (findMin(h) min findMin(other))
  }
  
  property("comb2") = forAll { (h: H, other: H) =>
    delMinAndCheck(meld(h, other)) == delMinAndCheck(meld(deleteMin(h), insert(findMin(h), other)))
  }

  private def delMinAndCheck(h: H): List[A] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: delMinAndCheck(deleteMin(h))
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
