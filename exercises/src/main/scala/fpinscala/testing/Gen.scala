package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import Prop._

trait SimpleProp { self =>
  def check: Boolean

  // Exercise 3: Assuming the following representation of Prop, implement && as a method of Prop.
  def &&(other: SimpleProp): SimpleProp =
    new SimpleProp {
      override def check: Boolean = self.check && other.check
    }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  // Exercise 9: Implement && and || for composing Pro values.
  def &&(p: Prop): Prop =
    Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case fail => fail
      }
    }


  def ||(p: Prop): Prop =
    Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(_, _) => p.run(max, n, rng)
        case pass => pass
      }
    }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }


  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}

case class Gen[+A](sample: State[RNG,A]) {
  def run(rng: RNG): A = sample.run(rng)._1

  // Exercise 6: Implement flatMap, and then use it to implement this more dynamic version of listOfN.
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  // Exercise 10: Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
  def unsized: SGen[A] =
    SGen(_ => this)
}

object Gen {
  // Exercise 4: Implement Gen.choose using this representation of Gen. It should generate integers in the range start to stopExclusive.
  // Feel free to use functions you’ve already written.
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.map(RNG.nonNegativeInt)(i => start + (i % (stopExclusive - start)))))

  // Exercise 5: Try implementing unit, boolean, and listOfN.
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def double: Gen[Double] =
    Gen(State(RNG.double2))

  def int: Gen[Int] =
    Gen(State(RNG.int))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // Exercise 13: Define listOf1 for generating nonempty lists, and then update your specification of max to use this generator.
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(s => listOfN(s max 1, g))

  // Exercise 7: Implement union, for combining two generators of the same type into one, by pulling values from each generator with equal likelihood.
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  // Exercise 8: Implement weighted , a version of union that accepts a weight for each Gen and generates values
  // from each Gen with probability proportional to its weight.
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (g1v, w1) = g1
    val (g2v, w2) = g2
    val g1prob = w1 / (w1 + w2)

    double.flatMap(d => if(d < g1prob) g1v else g2v)
  }

  // Exercise 12: Implement a listOf combinator that doesn't accept an explicit size. It should return an SGen instead of a Gen.
  // The implementation should generate lists of the requested size.
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(s => listOfN(s, g))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  // Exercise 11: Define some convenience functions on SGen that simply delegate to the corresponding functions on Gen
  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(s => forSize(s).flatMap(f))
}

object Examples {
  // Exercise 14: Write a property to verify the behavior of List.sorted
  def isSorted: Prop = {
    forAll(Gen.listOf1(Gen.int)) { is =>
      val sorted = is.sorted
      (sorted zip sorted.tail).forall(i => i._1 < i._2)
    }
  }
}
