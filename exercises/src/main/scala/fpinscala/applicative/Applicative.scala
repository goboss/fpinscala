package fpinscala
package applicative

import monads.Functor
import state._
import State._
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  // Exercise 2: Define apply in terms of map2 and unit. Define map and map2 in terms of apply and unit
  def unit[A](a: => A): F[A]
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply[A, B](unit(f))(fa)

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply[B, C](map(fa)(f.curried))(fb)

  // Exercise 3: Implement map3 and map4 using only unit, apply, and the curried method available on functions.
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply[C, D](map2(fa, fb)(f.curried(_)(_)))(fc)

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply[D, E](map3(fa, fb, fc)(f.curried(_)(_)(_)))(fd)

  // Exercise 1: Transplant the implementations of as many combinators as you can from Monad to Applicative,
  // using only map2 and unit, or methods implemented in terms of them.
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A]))((fa, fla) => map2(fa, fla)(_ :: _))

  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = ???

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = ???

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ???
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A,B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  // Exercise 5: Write a monad instance for Either.
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] =
      ma match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  // Exercise 6: Write an Applicative instance for Validation that accumulates errors in Failure.
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] =
      Success(a)
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) =>
          Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) =>
          Failure(h1, t1 ++ (h2 +: t2))
        case (f @ Failure(_, _), _) =>
          f
        case (_, f @ Failure(_, _)) =>
          f
      }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}
