package fpinscala.testing

import fpinscala.state.RNG.SimpleRNG
import fpinscala.testing.Prop.{Falsified, Proved}
import org.scalatest.{FlatSpec, Matchers}

class PropSpec extends FlatSpec with Matchers {

  // Exercise 3

  "A SimpleProp" should "calculate the conjunction of props" in {
    val p1 = new SimpleProp {
      override def check: Boolean = true
    }
    val p2 = new SimpleProp {
      override def check: Boolean = true
    }
    val p3 = new SimpleProp {
      override def check: Boolean = false
    }

    (p1 && p2).check shouldBe true
    (p1 && p3).check shouldBe false
    (p1 && p2 && p3).check shouldBe false
  }

  // Exercise 9

  "A Prop" should "calculate the conjunction of props" in {
    val p1 = Prop.check(true)
    val p2 = Prop.check(true)
    val p3 = Prop.check(false)

    (p1 && p2).run(2, 10, SimpleRNG(1)) shouldBe Proved
    (p1 && p3).run(2, 10, SimpleRNG(1)) shouldBe Falsified("()", 0)
    (p1 && p2 && p3).run(2, 10, SimpleRNG(1)) shouldBe Falsified("()", 0)
  }

  it should "calculate the alternative of props" in {
    val p1 = Prop.check(false)
    val p2 = Prop.check(false)
    val p3 = Prop.check(true)

    (p1 || p2).run(2, 10, SimpleRNG(1)) shouldBe Falsified("()", 0)
    (p1 || p3).run(2, 10, SimpleRNG(1)) shouldBe Proved
    (p1 || p2 || p3).run(2, 10, SimpleRNG(1)) shouldBe Proved
  }

}
