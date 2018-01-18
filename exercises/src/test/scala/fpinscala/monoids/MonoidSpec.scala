package fpinscala.monoids

import java.util.concurrent.Executors

import fpinscala.datastructures.{Branch, Leaf}
import fpinscala.parallelism.Nonblocking.Par
import fpinscala.state.RNG.SimpleRNG
import fpinscala.testing.Gen
import fpinscala.testing.Prop.{Falsified, Passed, Proved}
import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers {
  import Monoid._

  private val propParams = (100, 100, SimpleRNG(123L))

  // Exercise 4

  "monoidLaws" should "falsify left/right identity" in {
    val fakeMonoid = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2
      override def zero: Int = 1
    }

    monoidLaws(fakeMonoid, Gen.int).run.tupled(propParams) shouldBe a[Falsified]
  }

  it should "falsify associativity" in {
    val fakeMonoid = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 - a2
      override def zero: Int = 0
    }

    monoidLaws(fakeMonoid, Gen.int).run.tupled(propParams) shouldBe a[Falsified]
  }

  // Exercise 1

  "the intAddition monoid" should "observe the monoid laws" in {
    monoidLaws(intAddition, Gen.int).run.tupled(propParams) shouldBe Passed
  }

  "the intMultiplication monoid" should "observe the monoid laws" in {
    monoidLaws(intMultiplication, Gen.int).run.tupled(propParams) shouldBe Passed
  }

  "the booleanOr monoid" should "observe the monoid laws" in {
    monoidLaws(booleanOr, Gen.boolean).run.tupled(propParams) shouldBe Proved
  }

  "the booleanAnd monoid" should "observe the monoid laws" in {
    monoidLaws(booleanAnd, Gen.boolean).run.tupled(propParams) shouldBe Proved
  }

  // Exercise 2

  "the optionMonoid" should "observe the monoid laws" in {
    monoidLaws(optionMonoid[Int], Gen.option(Gen.int)).run.tupled(propParams) shouldBe Passed
  }

  // Exercise 5

  "A Monoid" should "map and combine a list" in {
    foldMap(List("1", "2", "3"), intAddition)(_.toInt) shouldBe 6
  }

  // Exercise 6

  it should "combine a list from the left" in {
    foldLeft(List("2", "3", "4"))(1)(_ * _.toInt) shouldBe 24
  }

  it should "combine a list from the right" in {
    foldRight(List("1", "2", "3"))(1)(_.toInt * _) shouldBe 6
  }

  // Exercise 7

  it should "map and combine an indexed seq" in {
    foldMapV(Vector(1, 2, 3), stringMonoid)(_.toString) shouldBe "123"
  }

  // Execise 8

  it should "map and combine a list in parallel" in {
    Par.run(Executors.newWorkStealingPool(10))(parFoldMap(Vector("1", "2", "3"), intAddition)(_.toInt)) shouldBe 6
  }

  // Exercise 9

  it should "creatively detect that a given list is ordered" in {
    ordered(Vector(1, 1, 3, 6)) shouldBe true
  }

  it should "creatively detect that a given list is not ordered" in {
    ordered(Vector(1, 1, 0, 6)) shouldBe false
  }

  // Exercise 10

  "the wcMonoid" should "observe the monoid laws" in {
    val gen = for {
      isStub <- Gen.boolean
      l <- Gen.alpha
      r <- Gen.alpha
      cnt <- Gen.choose(0, 100)
    } yield if (isStub) Stub(l + r) else Part(l, cnt, r)

    monoidLaws(wcMonoid, gen).run.tupled(propParams) shouldBe Passed
  }

  // Exercise 11

  "A Monoid" should "count the number of words in a string" in {
    count("lorem ipsum dolor sit amet, ") shouldBe 5
  }

  // Exercise 12

  "A ListFoldable" should "foldRight" in {
    ListFoldable.foldRight(List(1, 2, 3))(0)(_ + _) shouldBe 6
  }

  it should "foldLeft" in {
    ListFoldable.foldLeft(List(1, 2, 3))(0)(_ + _) shouldBe 6
  }

  it should "foldMap" in {
    ListFoldable.foldMap(List(1, 2, 3))(_.toString)(stringMonoid) shouldBe "123"
  }

  // Exercise 13

  "A TreeFoldable" should "foldRight" in {
    TreeFoldable.foldRight(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(0)(_ + _) shouldBe 6
  }

  it should "foldLeft" in {
    TreeFoldable.foldLeft(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(0)(_ + _) shouldBe 6
  }

  it should "foldMap" in {
    TreeFoldable.foldMap(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_.toString)(stringMonoid) shouldBe "123"
  }

  // Exercise 14

  "An OptionFoldable" should "foldRight" in {
    OptionFoldable.foldRight(Some(1))(1)(_ + _) shouldBe 2
    OptionFoldable.foldRight(Option.empty[Int])(1)(_ + _) shouldBe 1
  }

  it should "foldLeft" in {
    OptionFoldable.foldLeft(Some(1))(1)(_ + _) shouldBe 2
    OptionFoldable.foldLeft(Option.empty[Int])(1)(_ + _) shouldBe 1
  }

  it should "foldMap" in {
    OptionFoldable.foldMap(Some("1"))(_.toInt)(intAddition) shouldBe 1
    OptionFoldable.foldMap(Option.empty[String])(_.toInt)(intAddition) shouldBe 0
  }

  // Exercise 15

  "A Foldable" should "turn itself into a list" in {
    ListFoldable.toList(List(1, 2, 3)) shouldBe List(1, 2, 3)
    TreeFoldable.toList(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe List(1, 2, 3)
    OptionFoldable.toList(Some(1)).toList shouldBe List(1)
    OptionFoldable.toList(None).toList shouldBe List()
  }

  // Exercise 16

  "the productMonoid" should "observe the monoid laws" in {
    monoidLaws(productMonoid(intMultiplication, stringMonoid), Gen.tuple2(Gen.int, Gen.alpha)).run.tupled(propParams) shouldBe Passed
  }

  // Exercise 18

  "A Monoid" should "create a bag from an indexed seq" in {
    bag(Vector("a", "b", "a", "ab")) shouldBe Map("a" -> 2, "b" -> 1, "ab" -> 1)
  }
}
