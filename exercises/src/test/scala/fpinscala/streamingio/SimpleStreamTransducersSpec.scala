package fpinscala.streamingio

import org.scalatest.{FlatSpec, Matchers}

class SimpleStreamTransducersSpec extends FlatSpec with Matchers {
  import SimpleStreamTransducers.Process._

  "A SimpleStreamTransducers Process" should "lift a function" in {
    lift((i: Int) => i.toString).apply(Stream(1, 2, 3)) shouldBe Stream("1", "2", "3")
  }

  // Exercise 1

  it should "take n elements" in {
    take(3).apply(Stream.continually(1)) shouldBe Stream(1, 1, 1)
  }

  it should "take a negative number of elements" in {
    take(-1).apply(Stream(1, 2, 3)) shouldBe Stream()
  }

  it should "take as many elements as possible" in {
    take(10).apply(Stream(1, 2, 3)) shouldBe Stream(1, 2, 3)
  }

  it should "take elements while condition is true" in {
    takeWhile[Int](_ < 3).apply(Stream(1, 2, 3)) shouldBe Stream(1, 2)
  }

  it should "not take any elements" in {
    takeWhile[Int](_ => false).apply(Stream.continually(1)) shouldBe Stream()
  }

  it should "drop n elements" in {
    drop(2).apply(Stream(1, 2, 3)) shouldBe Stream(3)
  }

  it should "drop a negative number of elements" in {
    drop(-1).apply(Stream(1, 2, 3)) shouldBe Stream(1, 2, 3)
  }

  it should "drop as many elements as possible" in {
    drop(10).apply(Stream(1, 2, 3)) shouldBe Stream()
  }

  it should "drop elements while condition is true" in {
    dropWhile[Int](_ < 3).apply(Stream(1, 2, 3)) shouldBe Stream(3)
  }

  it should "not drop any elements" in {
    dropWhile[Int](_ => false).apply(Stream(1, 2, 3)) shouldBe Stream(1, 2, 3)
  }

  // Exercise 2

  it should "count the number of input elements" in {
    count.apply(Stream.empty[Int]) shouldBe Stream()
    count.apply(Stream('a', 'b', 'c')) shouldBe Stream(1, 2, 3)
  }

  // Exercise 3

  it should "calculate a mean" in {
    mean.apply(Stream(1.0, 2.0, 3.0)) shouldBe Stream(1.0, 1.5, 2.0)
  }

  // Exercise 4

  it should "calculate the sum of input elements using loop" in {
    sum2.apply(Stream(1.0, 2.0, -3.0)) shouldBe Stream(1.0, 3.0, 0.0)
  }

  it should "count the number of input elements using loop" in {
    count2.apply(Stream.empty[Int]) shouldBe Stream()
    count2.apply(Stream('a', 'b', 'c')) shouldBe Stream(1, 2, 3)
  }

  // Exercise 5

  it should "pipe to another Process" in {
    (drop[Double](5) |> take(5) |> sum).apply(Stream.from(1).map(_.toDouble)) shouldBe Stream(6.0, 13.0, 21.0, 30.0)
  }

  // Exercise 6

  it should "zip input elements with index" in {
    id.zipWithIndex.apply(Stream('a', 'b', 'c')) shouldBe Stream(('a', 0), ('b', 1), ('c', 2))
  }

  // Exercise 7

  it should "zip two Processes together" in {
    zip(sum, count[Double]).apply(Stream(1.0, 2.0, 3.0)) shouldBe Stream((1, 1), (3, 2), (6, 3))
  }

  // Exercise 8

  it should "check for the existence of an element that matches condition" in {
    exists[Int](_ > 2).apply(Stream(1, 2, 3)) shouldBe Stream(false, false, true)
  }
}
