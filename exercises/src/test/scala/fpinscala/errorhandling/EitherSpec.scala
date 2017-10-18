package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  // Exercise 6

  "Either" should "not map the value inside Left" in {
    Left(true).map(_ => false) shouldBe Left(true)
  }

  it should "map the value inside Right" in {
    Right(true).map(_ => false) shouldBe Right(false)
  }

  it should "not flatMap the value inside Left" in {
    Left(true).flatMap(_ => Right(false)) shouldBe Left(true)
    Left(true).flatMap(_ => Left(false)) shouldBe Left(true)
  }

  it should "flatMap the value inside Right" in {
    Right(true).flatMap(v => Right(!v)) shouldBe Right(false)
    Right(true).flatMap(v => Left(!v)) shouldBe Left(false)
  }

  it should "get the first Right in a chain of orElse invocations" in {
    Left(1) orElse Left(2) orElse Right(3) orElse Left(4) shouldBe Right(3)
  }

  it should "not map values inside two Eithers, one of which is Left" in {
    Left(1).map2(Right(2))((_, _) => 3) shouldBe Left(1)
    Right(1).map2(Left(2))((_, _) => 3) shouldBe Left(2)
  }

  it should "map values inside two Rights" in {
    Right(1).map2(Right(2))(_ + _) shouldBe Right(3)
  }

  // Exercise 7

  it should "sequence an empty list" in {
    Either.sequence(List.empty[Either[String, Int]]) shouldBe Right(List.empty[Int])
  }

  it should "sequence a list of Eithers, one of which is Left" in {
    Either.sequence(List(Right(1), Left(2), Right(3))) shouldBe Left(2)
  }

  it should "sequence a list of Rights" in {
    Either.sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
  }

  it should "traverse an empty list" in {
    Either.traverse(List.empty[Int])(i => Right(i)) shouldBe Right(List.empty[Int])
  }

  it should "traverse a list of values with a function that returns Left for some of them" in {
    Either.traverse(List(1, 2, 3))(i => if (i % 2 == 0) Left("nope") else Right("yes")) shouldBe Left("nope")
  }

  it should "traverse a list of values with a function that always returns a Right" in {
    Either.traverse(List("one", "two", "three"))(str => Right(str.length)) shouldBe Right(List(3, 3, 5))
  }

}
