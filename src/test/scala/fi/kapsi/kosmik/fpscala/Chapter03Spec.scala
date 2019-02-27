package fi.kapsi.kosmik.fpscala

import fi.kapsi.kosmik.fpscala.Chapter03.{Branch, Leaf}
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

  describe("Ex 14") {
    import Chapter03.Ex14._

    it("should append") {
      appendFL(List(1, 2, 3), 4) shouldEqual List(1, 2, 3, 4)
    }
  }

  describe("Ex 15") {
    import Chapter03.Ex15._

    it("should flatten") {
      flatten(List(List(1), List(2, 3), List(), List(4, 5))) shouldEqual List(1, 2, 3, 4, 5)
    }
  }

  describe("Ex 24") {
    import Chapter03.Ex24._

    it("should match") {
      hasSubSequence(List(1), List(1)) shouldEqual true
      hasSubSequence(List(1, 8, 2, 0), List(0)) shouldEqual true
      hasSubSequence(List(1, 8, 2, 0), List(8, 2)) shouldEqual true
      hasSubSequence(List(1, 8, 2, 0), List(1, 8, 2, 0)) shouldEqual true
    }

    it("should not match") {
      hasSubSequence(List(2), List(1)) shouldEqual false
      hasSubSequence(List(2), List(2, 2)) shouldEqual false
      hasSubSequence(List(2, 5, 1, 6), List(5, 1, 1)) shouldEqual false
    }
  }

  describe("Ex 25") {
    import Chapter03.Ex25.{size => tsize}

    it("should count size of tree") {
      tsize(Leaf(1)) shouldEqual 1
      tsize(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldEqual 5
    }
  }

  describe("Ex 26") {
    import Chapter03.Ex26._

    it("should get maximum value") {
      maximum(Leaf(1)) shouldEqual 1
      maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldEqual 3
    }
  }

  describe("Ex 27") {
    import Chapter03.Ex27._

    it("should get depth") {
      depth(Leaf(1)) shouldEqual 1
      depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldEqual 3
    }
  }

  describe("Ex 28") {
    import Chapter03.Ex28._

    it("should map tree") {
      map(Leaf(1))(_ * 2) shouldEqual Leaf(2)
      map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ * 2) shouldEqual Branch(Leaf(2), Branch(Leaf(4), Leaf(6)))
    }
  }

  describe("Ex 29") {
    import Chapter03.Ex29.{size => tsize, maximum, depth, map}

    it("should count size of tree") {
      tsize(Leaf(1)) shouldEqual 1
      tsize(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldEqual 5
    }

    it("should get maximum value") {
      maximum(Leaf(1)) shouldEqual 1
      maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldEqual 3
    }

    it("should get depth") {
      depth(Leaf(1)) shouldEqual 1
      depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldEqual 3
    }

    it("should map tree") {
      map(Leaf(1))(_ * 2) shouldEqual Leaf(2)
      map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ * 2) shouldEqual Branch(Leaf(2), Branch(Leaf(4), Leaf(6)))
    }
  }

}
