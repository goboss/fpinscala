package fpinscala.testing

import java.util.concurrent.Executors

import fpinscala.state.RNG
import fpinscala.state.RNG.SimpleRNG
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class GenSpec extends FlatSpec with Matchers with PropertyChecks {

  // Exercise 4

  "Gen" should "generate integers from the given range" in {
    forAll { seed: Long =>
      val result = Gen.choose(-10, 10).sample.run(SimpleRNG(seed))._1
      result should be >= -10
      result should be < 10
    }
  }

  // Exercise 5

  it should "generate constant value (unit)" in {
    forAll { seed: Long =>
      val result = Gen.choose(-10, 10).sample.run(SimpleRNG(seed))._1
      Gen.unit(1).sample.run(SimpleRNG(seed))._1 shouldBe 1
    }
  }

  it should "generate boolean values" in {
    Gen.boolean.sample.run(SimpleRNG(0))._1 shouldBe true
    Gen.boolean.sample.run(SimpleRNG(2))._1 shouldBe false
  }

  it should "generate a random list of size n" in {
    forAll { (seed: Long, n: Byte) =>
      whenever(n > 0) {
        Gen.listOfN(n.toInt, Gen.boolean).sample.run(SimpleRNG(seed))._1 should have size n
      }
    }
  }

  // Exercise 6

  it should "implement flatMap" in {
    Gen.choose(0, 99).flatMap(i => Gen.unit(i % 2 == 0)).sample.run(SimpleRNG(0))._1 shouldBe true
  }

  it should "generate a list from Gen using flatMap" in {
    forAll { (seed: Long, n: Byte) =>
      whenever(n > 0) {
        Gen.boolean.listOfN(n.toInt).sample.run(SimpleRNG(seed))._1 should have size n
      }
    }
  }

  // Exercise 7

  it should "combine two generators by pulling values from each with equal likelihood" in {
    val (result, _) = (0 until 100).foldLeft((0, SimpleRNG(0): RNG)) { (acc, _) =>
      Gen.union(Gen.unit(1), Gen.unit(-1)).sample.run(acc._2)
    }

    result shouldBe (0 +- 3)
  }

  // Exercise 8

  it should "combine two generators by pulling values from each with equal likelihood using weighted" in {
    val (result, _) = (0 until 100).foldLeft((0, SimpleRNG(0): RNG)) { (acc, _) =>
      Gen.weighted((Gen.unit(1), 1.0), (Gen.unit(-1), 1.0)).sample.run(acc._2)
    }

    result shouldBe (0 +- 3)
  }

  it should "combine two generators by pulling values from each with likelihood according to weights" in {
    val (result, _) = (0 until 100).foldLeft((0.0, SimpleRNG(0): RNG)) { (acc, _) =>
      Gen.weighted((Gen.unit(0.25), 4.0), (Gen.unit(-1.0), 1.0)).sample.run(acc._2)
    }

    result shouldBe (0.0 +- 3)
  }

  // Exercise 11

  "SGen" should "implement flatMap" in {
    Gen.choose(0, 99).unsized.flatMap(i => Gen.unit(i % 2 == 0)).forSize(1).sample.run(SimpleRNG(0))._1 shouldBe true
  }

  it should "zip two SGen together" in {
    (Gen.unit(1).unsized ** Gen.unit(2).unsized).forSize(1).sample.run(SimpleRNG(0))._1 shouldBe (1, 2)
  }

  // Exercise 12

  it should "generate a sized list of elements" in {
    forAll { (seed: Long, sizeSeed: Int) =>
      whenever(sizeSeed != Int.MinValue) {
        val s = (math.abs(sizeSeed) % 100) + 1
        Gen.listOf(Gen.boolean).forSize(s).sample.run(SimpleRNG(seed))._1 should have size s
      }
    }
  }

  // Exercise 13

  it should "generate a sized, non empty list of elements" in {
    forAll { (seed: Long, sizeSeed: Int) =>
      whenever(sizeSeed != Int.MinValue) {
        val s = math.abs(sizeSeed) % 100
        Gen.listOf1(Gen.boolean).forSize(s).sample.run(SimpleRNG(seed))._1 should have size math.max(1, s)
      }
    }
  }

  // Exercise 16

  it should "generate Par[Int]" in {
    val executor = Executors.newCachedThreadPool()
    forAll { seed: Long =>
      Gen.parInt.sample.run(SimpleRNG(seed))._1(executor).get() shouldBe an[Integer]
    }
  }

  // Exercise 19

  it should "generate a function A => B that uses a mapping from n elements" in {
    Gen.funN[Int, Boolean](2, Gen.boolean).sample.run(SimpleRNG(1))._1(0) shouldBe true
  }

}
