package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  // Exercise 1

  "Option" should "map a None" in {
    None.map(identity[Int]) shouldBe None
  }

  it should "map the value inside Some" in {
    Some("Guy").map(_.length * 2) shouldBe Some(6)
  }

  it should "flatMap a None" in {
    None.flatMap(identity) shouldBe None
  }

  it should "flatMap the value inside Some" in {
    Some("Thing").flatMap(str => Some(str.toLowerCase)) shouldBe Some("thing")
  }

  it should "flatMap the value inside Some to None" in {
    Some("Thing").flatMap(_ => None) shouldBe None
  }

  it should "get the default value when calling getOrElse on None" in {
    None.getOrElse(true) shouldBe true
  }

  it should "get the value inside Some when calling getOrElse on it" in {
    Some(42).getOrElse(0) shouldBe 42
  }

  it should "get the first Some in a chain of orElse invocations" in {
    None orElse None orElse Some(3) orElse None shouldBe Some(3)
  }

  it should "filter a None" in {
    None.filter(_ => true) shouldBe None
  }

  it should "filter out the value inside Some" in {
    Some(2).filter(_ < 1) shouldBe None
  }

  it should "keep the value inside Some when filter passes" in {
    Some(2).filter(_ < 10) shouldBe Some(2)
  }

  // Exercise 2

  it should "calculate variance of a non empty sequence of doubles" in {
    Option.variance(Seq(1.5, 3.0)) shouldBe Some(0.5625)
  }

  it should "not calculate variance of an empty sequence of doubles" in {
    Option.variance(Seq.empty) shouldBe None
  }

  // Exercise 3

  it should "map two Options, at least one of which is None" in {
    Option.map2(None, None)((_, _) => true) shouldBe None
    Option.map2(Some(1), None)((_, _) => true) shouldBe None
    Option.map2(None, Some(1))((_, _) => true) shouldBe None
  }

  it should "map two Some values with a binary function" in {
    Option.map2(Some(1), Some(2))(_ - _) shouldBe Some(-1)
  }

  // Exercise 4

  it should "combine an empty list of Option values into Some with empty list" in {
    Option.sequence(List.empty[Option[Int]]) shouldBe Some(List.empty[Option[Int]])
  }

  it should "combine a list of Option values, some of which are None, into None" in {
    Option.sequence(List(Some(1), None, Some(3))) shouldBe None
  }

  it should "combine a list of Some values into a Some of list" in {
    Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  }

  // Exercise 5

  it should "traverse an empty list of Int values and get Some with empty list" in {
    Option.traverse(List.empty[Int])(_ => Some(1)) shouldBe Some(List.empty[Int])
  }

  it should "traverse a list of Int values and get Some with a list of doubled values" in {
    Option.traverse(List(1, 2, 3))((x: Int) => Some(x * 2)) shouldBe Some(List(2, 4, 6))
  }

  it should "combine an empty list of Option values into Some with empty list using traverse" in {
    Option.sequence2(List.empty[Option[Int]]) shouldBe Some(List.empty[Option[Int]])
  }

  it should "combine a list of Option values, some of which are None, into None using traverse" in {
    Option.sequence2(List(Some(1), None, Some(3))) shouldBe None
  }

  it should "combine a list of Some values into a Some of list using traverse" in {
    Option.sequence2(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  }
}
