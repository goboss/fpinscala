package fpinscala.parallelism

import java.util.concurrent.Executors

import org.scalatest.concurrent.JavaFutures
import org.scalatest.{FlatSpec, Matchers}

class ParSpec extends FlatSpec with Matchers with JavaFutures {
  private val executor = Executors.newFixedThreadPool(2)
  private def testParResult[A, B](p: Par.Par[A])(test: A => B) = whenReady(Par.run(executor)(p))(test)

  // Exercise 3

  "Par" should "map 2 Par values" in {
    testParResult(Par.map2(Par.unit(1), Par.unit(2))(_ + _))(_ shouldBe 3)
  }

  // Exercise 4

  it should "lift a function into Par" in {
    testParResult(Par.asyncF((a: Int) => a * 2)(3))(_ shouldBe 6)
  }

  // Exercise 5

  it should "turn a list of Par values into Par of list" in {
    testParResult(Par.sequence(List(Par.unit(1), Par.unit(2))))(_ shouldBe List(1, 2))
  }

  // Exercise 6

  it should "filter elements of a list in parallel" in {
    testParResult(Par.parFilter(List(1, 2, 3, 4))(_ % 2 == 0))(_ shouldBe List(2, 4))
  }
}
