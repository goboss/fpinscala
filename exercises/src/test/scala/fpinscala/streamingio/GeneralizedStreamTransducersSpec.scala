package fpinscala.streamingio

import org.scalatest.{FlatSpec, Matchers}

class GeneralizedStreamTransducersSpec extends FlatSpec with Matchers {
  import GeneralizedStreamTransducers.Process._

  private type Id[A] = A

  private implicit val idMC = new MonadCatch[Id] {
    override def attempt[A](a: Id[A]): Id[Either[Throwable, A]] = try { Right(a) } catch { case t: Throwable => Left(t) }
    override def fail[A](t: Throwable): Id[A] = throw t
    override def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }

  private final class ExpectedError extends Exception("who let the dogs out?")

  // Exercise 10

  "A GeneralizedStreamTransducers Process" should "collect all output while running" in {
    emit[Id, Int](1, emit(2, emit(3))).runLog shouldBe Seq(1, 2, 3)
  }

  it should "await input by attempting the MonadCatch" in {
    emit[Id, Int](1, emit(2, await[Id, Int, Int](3){
      case Left(e) => fail(e)
      case Right(i) => emit(i)
    })).runLog shouldBe Seq(1, 2, 3)
  }

  it should "handle errors as per MonadCatch implementation" in {
    an[ExpectedError] should be thrownBy emit[Id, Int](1, emit(2, Halt(new ExpectedError))).runLog
  }

  // Exercise 12

  it should "join nested Processes" in {
    join(emit[Id, GeneralizedStreamTransducers.Process[Id, Int]](emit[Id, Int](1))).runLog shouldBe Seq(1)
  }

}
