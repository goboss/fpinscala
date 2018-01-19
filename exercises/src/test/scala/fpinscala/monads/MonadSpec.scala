package fpinscala.monads

import fpinscala.state.RNG.SimpleRNG
import fpinscala.testing.Prop.Passed
import fpinscala.testing.{Gen, Prop}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.higherKinds

class MonadSpec extends FlatSpec with Matchers {
  import Monad._

  private val propParams = (100, 100, SimpleRNG(123L))

  private def leftIdentityLaw[M[_], A, B](m: Monad[M], gen: Gen[A])(f: A => M[B]): Prop =
    Prop.forAll(gen) { a =>
      m.flatMap[A, B](m.unit(a))(f) == f(a)
    }

  private def rightIdentityLaw[M[_], A](m: Monad[M], gen: Gen[M[A]]): Prop =
    Prop.forAll(gen) { ma =>
      def f(a: A): M[A] = m.unit(a)
      m.flatMap[A, A](ma)(f) == ma
    }

  private def associativityLaw[M[_], A, B, C](m: Monad[M], gen: Gen[M[A]])(f: A => M[B])(g: B => M[C]): Prop =
    Prop.forAll(gen) { ma =>
      m.flatMap(m.flatMap(ma)(f))(g) == m.flatMap(ma)(a => m.flatMap(f(a))(g))
    }

  // Exercise 1

  "the optionMonad" should "observe the monad laws" in {
    val gen = Gen.option(Gen.int)
    rightIdentityLaw(optionMonad, gen).run.tupled(propParams) shouldBe Passed
    leftIdentityLaw(optionMonad, gen)(_.map(_ + 1)).run.tupled(propParams) shouldBe Passed
    associativityLaw(optionMonad, gen)(i => Some(i + 1))(i => Some(i.toString)).run.tupled(propParams) shouldBe Passed
  }

  "the streamMonad" should "observe the monad laws" in {
    val gen = Gen.listOfN(10, Gen.int).map(_.toStream)
    rightIdentityLaw(streamMonad, gen).run.tupled(propParams) shouldBe Passed
    leftIdentityLaw(streamMonad, gen)(_.map(_.toString)).run.tupled(propParams) shouldBe Passed
    associativityLaw(streamMonad, gen)(i => Stream.continually(i).take(10))(i => Stream(i.toString)).run.tupled(propParams) shouldBe Passed
  }

  "the listMonad" should "observe the monad laws" in {
    val gen = Gen.listOfN(10, Gen.alphaChar)
    rightIdentityLaw(listMonad, gen).run.tupled(propParams) shouldBe Passed
    leftIdentityLaw(listMonad, gen)(_.map(_.toString)).run.tupled(propParams) shouldBe Passed
    associativityLaw(listMonad, gen)(c => List(c.toUpper))(c => List(c.toString, c.toString)).run.tupled(propParams) shouldBe Passed
  }

  // Exercise 3

  "A Monad" should "be created generically from a sequence of monads" in {
    optionMonad.sequence(List(Some(1), Some(2))) shouldBe Some(List(1, 2))
    optionMonad.sequence(List(Some(1), None, Some(2))) shouldBe None
  }

  it should "traverse a list and build up a monad" in {
    optionMonad.traverse(List(1, 2, 3))(i => Some(-i)) shouldBe Some(List(-1, -2, -3))
    optionMonad.traverse(List(1, 2, 3))(i => if (i % 2 != 0) Some(i) else None) shouldBe None
  }

  // Exercise 4

  it should "replicate the given monad n times" in {
    optionMonad.replicateM(3, Some(1)) shouldBe Some(List(1, 1, 1))
    optionMonad.replicateM(3, None) shouldBe None
  }

  // Exercise 6

  it should "filter elements of a list in the context of the given monad" in {
    optionMonad.filterM(List(1, 2, 3))(i => Some(i % 2 == 0)) shouldBe Some(List(2))
  }

  // Exercise 7

  it should "compose two monadic functions" in {
    listMonad.compose((i: Int) => List.fill(i)('a'), (c: Char) => List(c.toUpper))(3) shouldBe List('A', 'A', 'A')
  }

  // Exercise 8

  it should "implement flatMap via compose" in {
    streamMonad.flatMapViaCompose(Stream('a', 'b', 'c'))(c => Stream(c, c.toUpper)) shouldBe Stream('a', 'A', 'b', 'B', 'c', 'C')
  }

  // Exercise 12

  it should "join nested monads" in {
    optionMonad.join(Some(Some(1))) shouldBe Some(1)
    optionMonad.join(Some(None)) shouldBe None
  }

  // Exercise 13

  it should "implement flatMap via join" in {
    streamMonad.flatMapViaJoin(Stream('a', 'b', 'c'))(c => Stream(c, c.toUpper)) shouldBe Stream('a', 'A', 'b', 'B', 'c', 'C')
  }

  // Exercise 17

  "Id" should "observe the monad laws" in {
    val gen = Gen.int.map(Id.apply)
    rightIdentityLaw(idMonad, gen).run.tupled(propParams) shouldBe Passed
    leftIdentityLaw(idMonad, gen)(_.map(_.toString)).run.tupled(propParams) shouldBe Passed
    associativityLaw(idMonad, gen)(i => Id(-i))(i => Id(i * 2)).run.tupled(propParams) shouldBe Passed
  }

  "Reader" should "should have unit" in {
    Reader.readerMonad[String].unit(3).run("test") shouldBe 3
  }

  it should "have flatMap" in {
    Reader.readerMonad[String].flatMap(Reader[String, Int](_.length))(_ => Reader[String, Double](_.length / 2.0)).run("test") shouldBe 2.0
  }
}
