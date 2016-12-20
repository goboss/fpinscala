package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.errorhandling._
import fpinscala.laziness._
import fpinscala.state._
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.testing.Gen._
import Prop._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

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
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
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
  def randomStream[A](g: Gen[A], rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop { (_, n, rng) =>
      @tailrec
      def loop(i: Int, s: Stream[A]): Result = s match {
        case Cons(h, t) if i < n =>
          Try(f(h())) match {
            case Success(fh) =>
              if (fh) loop(i + 1, t()) else Falsified(h().toString, i)
            case Failure(e) =>
              Falsified(buildMsg(h(), e), i)
          }
        case Empty =>
          Proved
        case _ =>
          Passed
      }

      loop(0, as.domain.getOrElse(randomStream(as, rng)))
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  val S: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  ) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get() }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Throwable): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}
