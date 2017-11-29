package fpinscala.state

import fpinscala.state.RNG.SimpleRNG
import fpinscala.state.State.{Coin, Machine, Turn}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers with PropertyChecks {

  // Exercise 1

  "A RNG" should "generate a random, non negative integer" in {
    RNG.nonNegativeInt(SimpleRNG(1)) shouldBe (384748,SimpleRNG(25214903928L))
  }

  // Exercise 2

  it should "generate a Double between 0 and 1 exclusive" in {
    forAll((seed: Long) => {
      val result = RNG.double(SimpleRNG(seed))._1
      result should be >= 0.0
      result should be < 1.0
    })
  }

  // Exercise 3

  it should "generate a random (Int, Double)" in {
    RNG.intDouble(SimpleRNG(1)) shouldBe ((384748,0.5360936469100852), SimpleRNG(206026503483683L))
  }

  it should "generate a random (Double, Int)" in {
    RNG.doubleInt(SimpleRNG(1)) shouldBe ((0.5360936469100852, 384748), SimpleRNG(206026503483683L))
  }

  it should "generate a random (Double, Double, Double)" in {
    RNG.double3(SimpleRNG(1)) shouldBe ((1.79162249052507E-4, 0.5360936469100852, 0.255826790004888), SimpleRNG(245470556921330L))
  }

  // Exercise 4

  it should "generate a list of random integers" in {
    RNG.ints(3)(SimpleRNG(1)) shouldBe (List(-549383847, -1151252339, 384748), SimpleRNG(245470556921330L))
  }

  // Exercise 5

  it should "generate double again, but this time using map" in {
    RNG.double2(SimpleRNG(1)) shouldBe (1.7916224896907806E-4, SimpleRNG(25214903928L))
  }

  // Exercise 6

  it should "map 2 RNG values" in {
    RNG.map2(RNG.int, RNG.double2)(_ * _)(SimpleRNG(1)) shouldBe (206260.9583653137, SimpleRNG(206026503483683L))
  }

  // Exercise 7

  it should "turn a list of RNG transitions into a single transition" in {
    RNG.sequence(List(RNG.int, RNG.int, RNG.boolean))(SimpleRNG(1)) shouldBe (List(384748, -1151252339, false), SimpleRNG(245470556921330L))
  }

  // Exercise 8

  it should "bind two RNG generators" in {
    val rand = RNG.flatMap(RNG.boolean)(b => if (b) RNG.unit(0) else RNG.int)
    rand(SimpleRNG(1)) shouldBe (0, SimpleRNG(25214903928L))
    rand(SimpleRNG(2)) shouldBe (1988230381, SimpleRNG(130300666313612L))
  }

  it should "generate non negative int that is less than the provided value" in {
    forAll { (n: Int, m: Long) =>
      whenever(n > 0) {
        RNG.nonNegativeLessThan(n)(SimpleRNG(m))._1 should be < n
      }
    }
  }

  // Exercise 9

  it should "map the RNG value using flatMap" in {
    RNG.mapViaFlatMap(RNG.nonNegativeInt)(_ * -1)(SimpleRNG(1)) shouldBe (-384748, SimpleRNG(25214903928L))
  }

  it should "map two RNG values using flatMap" in {
    RNG.map2ViaFlatMap(RNG.int, RNG.double2)(_ * _)(SimpleRNG(1)) shouldBe (206260.9583653137, SimpleRNG(206026503483683L))
  }

  // Exercise 10

  "A State" should "generalize unit" in {
    State.unit(2).run("test") shouldBe (2, "test")
  }

  it should "generalize map" in {
    State.unit(1).map(_ + 1).run("test") shouldBe (2, "test")
  }

  it should "generalize map2" in {
    State.unit(1).map2(State.unit[String, Int](2))(_ + _).run("test") shouldBe (3, "test")
  }

  it should "generalize flatMap" in {
    State.unit(false).flatMap(b => State.unit[String, Boolean](!b)).run("opposite day") shouldBe (true, "opposite day")
  }

  it should "generalize sequence" in {
    State.sequence(List(State.unit[String, Int](1), State.unit[String, Int](2))).run("test") shouldBe (List(1, 2), "test")
  }

  // Exercise 11

  "A State based candy machine" should "dispense a single candy" in {
    val s = State.simulateMachine(List(Coin, Turn)).run(Machine(locked = true, candies = 1, coins = 0))
    s shouldBe ((0, 1), Machine(locked = true, candies = 0, coins = 1))
  }

  it should "not dispense candy without coins" in {
    val s = State.simulateMachine(List(Turn, Turn)).run(Machine(locked = true, candies = 1, coins = 0))
    s shouldBe ((1, 0), Machine(locked = true, candies = 1, coins = 0))
  }

  it should "dispense multiple candies" in {
    val s = State.simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(locked = true, candies = 3, coins = 0))
    s shouldBe ((1, 2), Machine(locked = true, candies = 1, coins = 2))
  }

  it should "not dispense more candy than it has" in {
    val s = State.simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(locked = true, candies = 1, coins = 0))
    s shouldBe ((0, 1), Machine(locked = true, candies = 0, coins = 1))
  }

  it should "require a turn after every coin" in {
    val s = State.simulateMachine(List(Coin, Coin, Turn, Turn)).run(Machine(locked = true, candies = 3, coins = 0))
    s shouldBe ((2, 1), Machine(locked = true, candies = 2, coins = 1))
  }

}
