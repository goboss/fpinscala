package fpinscala.parallelism

import java.util.concurrent.Executors

import org.scalatest.{FlatSpec, Matchers}

class NonblockingSpec extends FlatSpec with Matchers {
  import Nonblocking.Par._

  private val executor = Executors.newFixedThreadPool(2)
  private def testParResult[A, B](p: Nonblocking.Par[A])(test: A => B) =
    test(Nonblocking.Par.run(executor)(p))

  // Exercise 11

  "Nonblocking Par" should "choose between N parallel computations based on the result of Par[Int]" in {
    testParResult(choiceN(unit(1))(List(unit("zero"), unit("one"))))(
      _ shouldBe "one")
  }

  it should "choose between 2 parallel computations based on the result of Par[Boolean]" in {
    testParResult(choiceViaChoiceN(unit(false))(unit(1), unit(2)))(_ shouldBe 2)
  }

  // Exercise 12

  it should "choose a parallel computation based on a key calculated by another Par" in {
    testParResult(
      choiceMap(unit("y"))(Map("x" -> unit("x"), "y" -> unit("y"))))(
      _ shouldBe "y")
  }

  // Exercise 13

  it should "choose a parallel computation based on the value of previous computation and function f (flatMap basically)" in {
    testParResult(chooser(unit(2))(i => unit(i * 3)))(_ shouldBe 6)
  }

  it should "choose between N parallel computations based on the result of Par[Int] using chooser" in {
    testParResult(choiceNChooser(unit(1))(List(unit("zero"), unit("one"))))(
      _ shouldBe "one")
  }

  it should "choose between 2 parallel computations based on the result of Par[Boolean] using chooser" in {
    testParResult(choiceViaChooser(unit(false))(unit(2), unit(1)))(_ shouldBe 2)
  }

  // Exercise 14

  it should "join nested Par computations" in {
    testParResult(join(unit(unit(1))))(_ shouldBe 1)
  }

  it should "join nested Par computations using flatMap" in {
    testParResult(joinViaFlatMap(unit(unit(2))))(_ shouldBe 2)
  }

  it should "implement flatMap using join" in {
    testParResult(flatMapViaJoin(unit(1))(i => unit(i + 3)))(_ shouldBe 4)
  }

}
