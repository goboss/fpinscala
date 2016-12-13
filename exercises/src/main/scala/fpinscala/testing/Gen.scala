package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait SimpleProp { self =>
  def check: Boolean

  // Exercise 3: Assuming the following representation of Prop, implement && as a method of Prop.
  def &&(other: SimpleProp): SimpleProp =
    new SimpleProp {
      override def check = self.check && other.check
    }
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG,A]) {
  // Exercise 6: Implement flatMap, and then use it to implement this more dynamic version of listOfN.
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))
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

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
}


trait SGen[+A] {

}

