package fi.kapsi.kosmik.fpscala

import org.scalatest.{FunSpec, Matchers}

class Chapter03Spec extends FunSpec with Matchers {
  describe("Exercise 13") {
    import Chapter03.Ex13._
    val list = List(1, 2, 3)

    it("foldRightFR should apply function on all items") {
      foldLeftFR(list, 0)(_ + _) shouldEqual 6
    }

    it("foldRightFL should apply function on all items") {
      foldRightFL(list, 0)(_ + _) shouldEqual 6
    }

    it("foldRightFL with cons should produce the same list") {
      list.foldRight(Nil: List[Int])((h, acc) => h :: acc) shouldEqual List(1, 2, 3)
      foldRightFL(list, Nil: List[Int])((h, acc) => h :: acc) shouldEqual List(1, 2, 3)
    }

    it("foldLeftFR with cons should reverse the list") {
      list.foldLeft(Nil: List[Int])((acc, h) => h :: acc) shouldEqual List(3, 2, 1)
      foldLeftFR(list, Nil: List[Int])((acc, h) => h :: acc) shouldEqual List(3, 2, 1)
    }
  }
}
