package fi.kapsi.kosmik.fpscala

import fi.kapsi.kosmik.fpscala.Chapter05._
import org.scalatest.{FunSpec, Matchers}

class Chapter05Spec extends FunSpec with Matchers {
  describe("Exercise 04") {
    import Chapter05.Ex04._
    // Let's cheat a bit and use functions from following exercises
    import Chapter05.Ex09.from
    import Chapter05.Ex13.take

    it("forAll") {
      forAll(take(from(1), 0))(_ => false) shouldBe true
      forAll(take(from(1), 1))(_ => false) shouldBe false
      forAll(take(from(1), 9))(_ < 10) shouldBe true
      forAll(from(1))(_ => false) shouldBe false
      forAll(from(1))(_ < 9) shouldBe false
    }
  }

  describe("Exercise 08") {
    import Chapter05.Ex08._

    it("constant") {
      toList(constant(5), 3) shouldEqual 5 :: 5 :: 5 :: Nil
    }
  }

  describe("Exercise 09") {
    import Chapter05.Ex09._

    it("from") {
      toList(from(5), 4) shouldEqual 5 :: 6 :: 7 :: 8 :: Nil
    }
  }

  describe("Exercise 10") {
    import Chapter05.Ex10._

    it("fibs") {
      toList(fibs, 6) shouldEqual 0 :: 1 :: 1 :: 2 :: 3 :: 5 :: Nil
    }
  }

  describe("Exercise 11") {
    import Chapter05.Ex11._

    it("unfold") {
      toList(unfold(1)(s => Some(s.toString, s * 2)), 4) shouldEqual "1" :: "2" :: "4" :: "8" :: Nil
      toList(unfold(1)(s => if (s >= 4) None else Some(s.toString, s + 1))) shouldEqual "1" :: "2" :: "3" :: Nil
    }
  }

  describe("Exercise 12") {
    import Chapter05.Ex12._

    it("unfold implementations for fibs, from, constant, ones") {
      toList(ones, 3) shouldEqual 1 :: 1 :: 1 :: Nil
      toList(constant(5), 3) shouldEqual 5 :: 5 :: 5 :: Nil
      toList(from(5), 4) shouldEqual 5 :: 6 :: 7 :: 8 :: Nil
      toList(fibs, 6) shouldEqual 0 :: 1 :: 1 :: 2 :: 3 :: 5 :: Nil
    }
  }

  describe("Exercise 13") {
    import Chapter05.Ex13._
    import Chapter05.Ex12.from

    it("unfold implementations for map, take, takeWhile, zipWith, zipAll") {
      toList(map(from(1))(_ * 2), 3) shouldEqual 2 :: 4 :: 6 :: Nil

      toList(take(from(2), 3)) shouldEqual 2 :: 3 :: 4 :: Nil

      toList(takeWhile(from(6))(_ < 9)) shouldEqual 6 :: 7 :: 8 :: Nil
      // takeWhile should be non-strict and not evaluate the whole input stream
      toList(take(takeWhile(from(6))(_ => true), 3)) shouldEqual 6 :: 7 :: 8 :: Nil

      toList(zipWith(from(2), 0), 3) shouldEqual (2, 0) :: (3, 0) :: (4, 0) :: Nil

      toList(zipAll(take(from(3), 2), take(from(7), 2))) shouldEqual (Some(3), Some(7)) :: (Some(4), Some(8)) :: Nil
      toList(zipAll(take(from(3), 3), take(from(7), 2))) shouldEqual (Some(3), Some(7)) :: (Some(4), Some(8)) :: (Some(5), None) :: Nil
      toList(zipAll(take(from(3), 4), take(from(7), 2))) shouldEqual (Some(3), Some(7)) :: (Some(4), Some(8)) :: (Some(5), None) :: (Some(6), None) :: Nil
      toList(zipAll(take(from(3), 2), take(from(7), 3))) shouldEqual (Some(3), Some(7)) :: (Some(4), Some(8)) :: (None, Some(9)) :: Nil
      toList(take(zipAll(from(3), from(7)), 2)) shouldEqual (Some(3), Some(7)) :: (Some(4), Some(8)) :: Nil
    }
  }

  describe("Exercise 14") {
    import Chapter05.Ex14._
    import Chapter05.Ex13.take
    import Chapter05.Ex12.from

    it("startsWith should be true on matching infinite and finite streams") {
      startsWith(from(1))(take(from(1), 3)) shouldBe true
    }

    it("startsWith should be false on matching finite and infinite streams") {
      startsWith(take(from(1), 2))(from(1)) shouldBe false
    }

    it("startsWith should be false on non-matching infinite and infinite streams") {
      startsWith(from(1))(from(2)) shouldBe false
    }

    it("startsWith should be true on matching finite streams of same length") {
      startsWith(take(from(1), 4))(take(from(1), 4)) shouldBe true
    }
  }

}
