package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.errorhandling.{Some, None}

class StreamSpec extends FlatSpec with Matchers {

  // Exercise 1

  "Stream" should "convert an empty Stream to a List" in {
    Stream.empty[Int].toList shouldBe List.empty[Int]
  }

  it should "convert non empty, but finite, Stream to a List" in {
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

  // Exercise 2

  it should "take the first few elements of an empty Stream" in {
    Stream.empty[Int].take(3) shouldBe Stream.empty[Int]
  }

  it should "take the first 0 elements of a non empty Stream" in {
    Stream("a", "b", "c").take(2).toList shouldBe List("a", "b")
  }

  it should "take the first few elements of a non empty Stream" in {
    Stream(1, 2, 3).take(2).toList shouldBe List(1, 2)
  }

  it should "drop the first few elements of an empty Stream" in {
    Stream.empty[Int].drop(3) shouldBe Stream.empty[Int]
  }

  it should "drop the first 0 elements of a non empty Stream" in {
    Stream("a", "b", "c").drop(2).toList shouldBe List("c")
  }

  it should "drop the first few elements of a non empty Stream" in {
    Stream(1, 2, 3).drop(2).toList shouldBe List(3)
  }

  // Exercise 3

  it should "take elements from a non empty Stream while a condition is true" in {
    Stream(1, 2, 3).takeWhile(_ < 3).toList shouldBe List(1, 2)
  }

  it should "take elements from a non empty Stream while it has more elements" in {
    Stream(1, 2, 3).takeWhile(_ => true).toList shouldBe List(1, 2, 3)
  }

  // Exercise 4

  it should "check that all elements of a non empty, finite Stream match a predicate" in {
    Stream(1, 2, 3).forAll(_ < 10) shouldBe true
  }

  it should "check that some elements a a non empty, infinite Stream match a predicate" in {
    Stream.from(0).forAll(_ < 10) shouldBe false
  }

  // Exercise 5

  it should "take elements from a non empty Stream while a condition is true using foldRight" in {
    Stream(1, 2, 3).takeWhile2(_ < 3).toList shouldBe List(1, 2)
  }

  it should "take elements from a non empty Stream while it has more elements using foldRight" in {
    Stream(1, 2, 3).takeWhile2(_ => true).toList shouldBe List(1, 2, 3)
  }

  // Exercise 6

  it should "return None as the head of an empty Stream" in {
    Stream.empty[Int].headOption2 shouldBe fpinscala.errorhandling.None
  }

  it should "return Some head of a non empty Stream" in {
    Stream(1, 2, 3).headOption2 shouldBe fpinscala.errorhandling.Some(1)
  }

  // Exercise 7

  it should "map an empty Stream" in {
    Stream.empty[String].map(_.length).toList shouldBe List.empty[Int]
  }

  it should "map a non empty Stream" in {
    Stream("zero", "one", "two").map(_.length).toList shouldBe List(4, 3, 3)
  }

  // Exercise 8

  it should "create an infinite Stream of a constant value" in {
    Stream.constant(42).take(1000).toList shouldBe List.fill(1000)(42)
  }

  // Exercise 9

  it should "create an infinite Stream of successive integers" in {
    Stream.from(0).take(10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  // Exercise 10

  it should "create an infinite Stream of successive fibonacci numbers" in {
    Stream.fibs.take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  // Exercise 11

  it should "unfold a seed into a finite Stream" in {
    Stream.unfold(0)(i => if (i < 10) Some((-i, i + 1)) else None).toList shouldBe List(0, -1, -2, -3, -4, -5, -6, -7, -8, -9)
  }

  it should "unfold a seed into an infinite Stream" in {
    Stream.unfold(0)(i => Some(i, i+1)).take(10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  // Exercise 12

  it should "create an infinite Stream of successive fibonacci numbers using unfold" in {
    Stream.fibs2.take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  // Exercise 13

  it should "map an empty Stream using unfold" in {
    Stream.map(Stream.empty[String])(_.length).toList shouldBe List.empty[Int]
  }

  it should "map a non empty Stream using unfold" in {
    Stream.map(Stream("zero", "one", "two"))(_.length).toList shouldBe List(4, 3, 3)
  }

  it should "take the first few elements of an empty Stream using unfold" in {
    Stream.take(Stream.empty[Int], 3) shouldBe Stream.empty[Int]
  }

  it should "take the first 0 elements of a non empty Stream using unfold" in {
    Stream.take(Stream("a", "b", "c"), 2).toList shouldBe List("a", "b")
  }

  it should "take the first few elements of a non empty Stream using unfold" in {
    Stream.take(Stream(1, 2, 3), 2).toList shouldBe List(1, 2)
  }

  it should "take elements from a non empty Stream while a condition is true using unfold" in {
    Stream.takeWhile(Stream(1, 2, 3))(_ < 3).toList shouldBe List(1, 2)
  }

  it should "take elements from a non empty Stream while it has more elements using unfold" in {
    Stream.takeWhile(Stream(1, 2, 3))(_ => true).toList shouldBe List(1, 2, 3)
  }

  it should "zip two Streams with the provided function until both are non empty" in {
    Stream.zipWith(Stream("one", "two"), Stream(1, 2, 3))(_ * _).toList shouldBe List("one", "twotwo")
  }

  it should "zip two Streams while at least one of them contains elements" in {
    Stream.zipAll(Stream("one", "two"), Stream(1, 2, 3)).toList shouldBe List((Some("one"), Some(1)), (Some("two"), Some(2)), (None, Some(3)))
  }

  // Exercise 14

  it should "check that a Stream contain a prefix" in {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 2, 3).startsWith(Stream(1, 3)) shouldBe false
  }

  // Exercise 15

  it should "calculate all the tails of a stream using unfold" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  // Exercise 16

  it should "fold a Stream and return all the intermediate results" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
  }
}