package fpinscala.testing

import fpinscala.state._
import fpinscala.laziness._
import fpinscala.errorhandling._
import Prop._

case class Gen[+A](sample: State[RNG, A], domain: Option[Stream[A]]) {
  // Exercise 6: Implement flatMap, and then use it to implement this more dynamic version of listOfN.
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(
      sample.flatMap(a => f(a).sample),
      domain.flatMap { dom =>
        dom.headOption.flatMap(a => f(a).domain)
      }
    )

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  // Exercise 10: Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
  def unsized: SGen[A] =
    SGen(_ => this)
}

object Gen {
  type Domain[A] = Option[Stream[A]]

  def unboundedDomain[A]: Domain[A] = None
  def boundedDomain[A](s: Stream[A]): Domain[A] = Some(s)

  // Exercise 4: Implement Gen.choose using this representation of Gen. It should generate integers in the range start to stopExclusive.
  // Feel free to use functions you’ve already written.
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(RNG.map(RNG.nonNegativeInt)(i => start + (i % (stopExclusive - start)))),
      boundedDomain(Stream.from(start).take(stopExclusive))
    )

  // Exercise 5: Try implementing unit, boolean, and listOfN.
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), boundedDomain(Stream(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean), boundedDomain(Stream(true, false)))

  def double: Gen[Double] =
    Gen(State(RNG.double2), unboundedDomain)

  // TODO: make this bounded
  def int: Gen[Int] =
    Gen(State(RNG.int), unboundedDomain)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)), unboundedDomain)

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
