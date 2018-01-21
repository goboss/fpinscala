package fpinscala.applicative

import org.scalatest.{FlatSpec, Matchers}

class ApplicativeSpec extends FlatSpec with Matchers {
  import Applicative._

  // Exercise 1

  "An Applicative" should "make a product" in {
    streamApplicative.product(Stream(1, 2), Stream(3, 4)) shouldBe Stream((1 ,3), (2, 4))
  }

  it should "make a sequence" in {
    streamApplicative.sequence(List(Stream(1, 2), Stream(3, 4))) shouldBe Stream(List(1, 3), List(2, 4))
  }

  it should "traverse a List" in {
    streamApplicative.traverse(List(1, 2, 3))(i => Stream(-i)) shouldBe Stream(List(-1, -2, -3))
  }

  // Exercise 2

  it should "apply a function inside Applicative" in {
    streamApplicative.apply(Stream((i: Int) => i + 1, (i: Int) => i - 1))(Stream(1, 2)) shouldBe Stream(2, 1)
  }

  it should "map an Applicative" in {
    streamApplicative.map(Stream("a", "b", "c"))(_.toUpperCase) shouldBe Stream("A", "B", "C")
  }

  it should "map 2 Applicatives" in {
    streamApplicative.map2(Stream(1, 2), Stream("one", "two"))(_ -> _) shouldBe Stream(1 -> "one", 2 -> "two")
  }

  // Exercise 3

  it should "map 3 Applicatives" in {
    streamApplicative.map3(Stream("this"), Stream("is"), Stream("Sparta!"))(_ + " " + _ + " " + _) shouldBe Stream("this is Sparta!")
  }

  it should "map 4 Applicatives" in {
    streamApplicative.map4(
      Stream(1, 2),
      Stream(10, 20),
      Stream(100, 200),
      Stream(1000, 2000)
    )(_ + _ + _ + _) shouldBe Stream(1111, 2222)
  }

  // Exercise 6

  "the validationApplicative" should "accumulate errors" in {
    validationApplicative[String].map3(
      Success("ok"),
      Failure("invalid username", Vector.empty),
      Failure("invalid password", Vector("are you really 400 years old?"))
    )((_, _, _) => "signed up") shouldBe Failure("invalid username", Vector("invalid password", "are you really 400 years old?"))
  }

  it should "map successes" in {
    validationApplicative[String].map2(
      Success("mr.hacker"),
      Success("god")
    )((uname, pwd) => s"lame confirmation email: $uname, $pwd") shouldBe Success("lame confirmation email: mr.hacker, god")
  }

  // Exercise 8

  "An Applicative" should "make a product of 2 Applicatives" in {
    streamApplicative.product(streamApplicative).apply((Stream((i: Int) => i + 1), Stream((i: Int) => i - 1)))(
      (Stream(1), Stream(2))
    ) shouldBe (Stream(2), Stream(1))
  }

  // Exercise 9

  it should "compose" in {
    val ap = streamApplicative.compose(streamApplicative)
    ap.apply(
      Stream(Stream((i: Int) => i + 1), Stream((i: Int) => i - 1))
    )(Stream(Stream(1), Stream(10))) shouldBe Stream(Stream(2), Stream(9))
  }


  // Exercise 12

  it should "sequence over a Map" in {
    streamApplicative.sequenceMap(Map("x" -> Stream(1, 2), "y" -> Stream(3, 4))) shouldBe Stream(Map("x" -> 1, "y" -> 3), Map("x" -> 2, "y" -> 4))
  }
}
