package fpinscala.testing

import fpinscala.state._
import fpinscala.laziness._
import fpinscala.errorhandling._
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import Prop._
import Gen._

case class Gen[+A](sample: State[RNG, A], domain: Option[Stream[A]]) {
  // Exercise 6: Implement flatMap, and then use it to implement this more dynamic version of listOfN.
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(
      sample.flatMap(a => f(a).sample),
      domain.flatMap { dom =>
        dom.headOption.flatMap(a => f(a).domain)
      }
    )

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f), unboundedDomain)

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f), unboundedDomain)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def listOfN(size: Int): Gen[List[A]] =
    listOfN(Gen.unit(size))

  def listOf1: SGen[List[A]] =
    Gen.listOf1(this)

  // Exercise 10: Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
  def unsized: SGen[A] =
    SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}

object Gen {
  type Domain[A] = Option[Stream[A]]

  private var DefaultSampleSize = 100

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

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

  def sequence[A](g: Seq[Gen[A]]): Gen[Seq[A]] =
    g.foldLeft(Gen.unit(Seq.empty[A])) { (acc, ga) =>
      acc.flatMap(as => ga.map(a => a +: as))
    }

  def sequenceSized[A](sg: SGen[Seq[Gen[A]]]): SGen[Seq[A]] =
    SGen { i =>
      sg.forSize(i).flatMap(sequence)
    }

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean), boundedDomain(Stream(true, false)))

  def double: Gen[Double] =
    Gen(State(RNG.double2), unboundedDomain)

  // TODO: make this bounded
  def int: Gen[Int] =
    Gen(State(RNG.int), unboundedDomain)

  def asciiN(n: Int): Gen[String] =
    choose(0, 128).listOfN(n).map(_.map(_.toChar).mkString)

  def ascii: Gen[String] =
    choose(0, DefaultSampleSize).flatMap(asciiN)

  def alphaChar: Gen[Char] =
    pick(IndexedSeq.range(48, 57) ++ IndexedSeq.range(65, 90) ++ IndexedSeq.range(97, 122)).map(_.toChar)

  def alphaN(n: Int): Gen[String] =
    alphaChar.listOfN(n).map(_.mkString)

  def alpha: Gen[String] =
    choose(0, DefaultSampleSize).flatMap(alphaN)

  def pick[A](xs: Seq[A]): Gen[A] =
    choose(0, xs.length).map(xs.apply)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)), g.domain.map(_.map(a => List.fill(n)(a))))

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

  // Exercise 16: Write a richer generator for Par[Int] , which builds more deeply nested parallel computations than the simple ones we gave previously.
  def parInt: Gen[Par[Int]] =
    int.listOfN(choose(0, 10)).map { l =>
      l.foldLeft(Par.unit(0)) { (acc, a) =>
        Par.fork(Par.map2(acc, Par.unit(a))(_ + _))
      }
    }

  def gen: Gen[Gen[_]] =
    pick(Seq(Gen.int, Gen.double, Gen.boolean, Gen.ascii, Gen.alpha))

  // Exercise 19: We want to generate a function that uses its argument in some way to select which Int to return.
  def funN[A, B](n: Int, g: Gen[B]): Gen[A => B] =
    g.listOfN(n).map(l => a => l(math.abs(a.hashCode) % l.length))

  def fun[A, B](g: Gen[B]): Gen[A => B] =
    funN(DefaultSampleSize, g)

  def funToInt[A](g: Gen[A]): Gen[A => Int] =
    fun(Gen.int)

  def funToString[A](g: Gen[A]): Gen[A => String] =
    fun(Gen.alpha)

  def funToBoolean[A](g: Gen[A]): Gen[A => Boolean] =
    fun(Gen.boolean)

  def funToListOfN[A, B](n: Int, g: Gen[B]): Gen[A => List[B]] =
    fun(Gen.listOfN(n, g))

  def option[A](g: Gen[A]): Gen[scala.Option[A]] =
    boolean.flatMap {
      case true => g.map(scala.Some.apply)
      case false => Gen.unit(scala.None)
    }

  def tuple2[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] =
    ga.flatMap(a => gb.map(b => (a, b)))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  // Exercise 11: Define some convenience functions on SGen that simply delegate to the corresponding functions on Gen
  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(s => forSize(s).flatMap(f))

  def map[B](f: A => B): SGen[B] =
    SGen(s => forSize(s).map(f))

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => forSize(n) ** s2.forSize(n))
}

object Examples {
  // Exercise 14: Write a property to verify the behavior of List.sorted
  val isSorted: Prop =
    forAll(Gen.listOf1(Gen.int)) { is =>
      val sorted = is.sorted
      (sorted zip sorted.tail).forall(i => i._1 < i._2)
    }

  // Exercise 17: Express the property about fork from chapter 7, that fork(x) == x.
  val forkProp: Prop =
    forAllPar(Gen.parInt) { i =>
      equal(Par.fork(i), i)
    }

  // Exercise 18: Come up with some other properties that takeWhile should satisfy. Can you think of a
  // good property expressing the relationship between takeWhile and dropWhile ?
  val takeWhileProp: Prop =
    forAll(Gen.int.listOf1 ** Gen.funToBoolean(Gen.int).unsized) { case (is, p) =>
      is.takeWhile(p) ++ is.dropWhile(p) == is
    }

  val takeProp: Prop =
    forAll(Gen.int.listOf1) { is =>
      is.take(is.size) == is
    }

  val dropProp: Prop =
    forAll(Gen.sequenceSized[Any](Gen.gen.listOf1)) { xs =>
      xs.drop(0) == xs
    }
}
