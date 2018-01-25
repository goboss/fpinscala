package fpinscala.gettingstarted

import org.scalatest.{FlatSpec, Matchers}

class GettingStartedSpec extends FlatSpec with Matchers {

  // Exercise 1

  "GettingStarted" should "compute the first 2 fibonacci numbers" in {
    MyModule.fib(0) shouldBe 0
    MyModule.fib(1) shouldBe 1
  }

  it should "compute nth fibonacci number" in {
    MyModule.fib(10) shouldBe 55
  }

  // Exercise 2

  it should "confirm that an empty array is sorted" in {
    PolymorphicFunctions.isSorted(Array.empty[Int], (_: Int) < (_: Int)) shouldBe true
  }

  it should "confirm that a non empty list is sorted" in {
    PolymorphicFunctions.isSorted(Array(1, 3, 4, 5), (_: Int) < (_: Int)) shouldBe true
  }

  it should "confirm that a non empty list is not sorted" in {
    PolymorphicFunctions.isSorted(Array(1, 3, 2, 5), (_: Int) < (_: Int)) shouldBe false
  }

  // Exercise 3

  it should "curry a function" in {
    PolymorphicFunctions.curry(math.max)(4)(3) shouldBe 4
  }

  // Exercise 4

  it should "uncurry a function" in {
    PolymorphicFunctions.uncurry((i: Int) => (s: String) => s * i)(3, "na") shouldBe "nanana"
  }

  // Exercise 5

  it should "compose two functions with aligned types" in {
    PolymorphicFunctions.compose(
      (d: Double) => d.toString,
      (i: Int) => i.toDouble)(12) shouldBe "12.0"
  }

}
