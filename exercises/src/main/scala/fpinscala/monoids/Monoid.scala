package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._

import language.higherKinds
import scala.List
import scala.math.min

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = Nil
  }

  // Exercise 1: Give Monoid instances for integer addition and multiplication as well as the Boolean operators.
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero: Boolean = true
  }

  // Exercise 2: Give a Monoid instance for combining Option values.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val zero: Option[A] = None
  }

  // Exercise 3: Write a monoid for endofunctions.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2
    val zero: (A) => A = identity[A]
  }

  def reverse[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    val zero: A = m.zero
  }

  import fpinscala.testing._
  import Prop._
  // Exercise 4: Use the property-based testing framework we developed in part 2 to implement a property for the monoid laws.
  // Use your property to test the monoids we’ve written.
  // Note: since we cannot test functions for equality endoMonoid cannot be tested.
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(Gen.listOfN(3, gen)) {
      case a1 :: a2 :: a3 :: Nil =>
        import m.op

        op(a1, m.zero) == a1 &&
        op(m.zero, a1) == a1 &&
        op(op(a1, a2), a3) == op(a1, op(a2, a3))
      case _ => false
    }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 5: Implement foldMap.
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))

  // Exercise 6: The foldMap function can be implemented using either foldLeft or foldRight.
  // But you can also write foldLeft and foldRight using foldMap! Try it.
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, reverse(endoMonoid[B]))(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  // Exercise 7: Implement a foldMap for IndexedSeq.
  // Your implementation should use the strategy of splitting the sequence in two,
  // recursively processing each half, and then adding the answers together with the monoid.
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.size < 2)
      as.headOption.map(f).getOrElse(m.zero)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  // Exercise 8: Also implement a parallel version of foldMap using the library we developed in chapter 7
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    val zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.flatMap(Par.sequenceBalanced(as.map(Par.asyncF(f)))) { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  // Exercise 9: Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll need to come up with a creative Monoid
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val creativeMonoid = new Monoid[(Option[Int], Boolean)] {
      def op(
        a1: (Option[Int], Boolean),
        a2: (Option[Int], Boolean)): (Option[Int], Boolean) =
        (a1, a2) match {
          case ((_, false), (i2, _)) =>
            (i2, false)
          case ((Some(i1), _), (Some(i2), _)) if i1 > i2 =>
            (Some(i2), false)
          case ((i1, _), _) =>
            (i1, true)
        }
      val zero: (Option[Int], Boolean) = (None, true)
    }

    foldMapV(ints, creativeMonoid)(i => (Some(i), true))._2
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10: Write a monoid instance for WC and make sure that it meets the monoid laws.
  def wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC =
      (a1, a2) match {
        case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
        case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
        case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          Part(l1, w1 + w2 + min(1, (r1 + l2).length), r2)
      }
    val zero: WC = Stub("")
  }

  // Exercise 11: Use the WC monoid to implement a function that counts words in a String
  // by recursively splitting it into substrings and counting the words in those substrings.
  def count(s: String): Int = {
    def countWord(w: String): Int = min(1, w.length)

    foldMapV(s.toVector, wcMonoid)(c =>
      if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)) match {
      case Stub(c) => countWord(c)
      case Part(l, w, r) => w + countWord(l) + countWord(r)
    }
  }

  // Exercise 16: Write a monoid instance for products.
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a1: (A, B), a2: (A, B)): (A, B) =
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      val zero: (A, B) = (A.zero, B.zero)
    }

  // Exercise 17: Write a monoid instance for functions whose results are monoids.
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[(A) => B] {
      def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))
      val zero: (A) => B = _ => B.zero
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
      val zero: Map[K, V] = Map[K, V]()
    }

  // Exercise 18: Use monoids to compute a “bag” from an IndexedSeq.
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(reverse(endoMonoid[B]))(z)

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 15: Any Foldable structure can be turned into a List. Write this conversion in a generic way.
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)
}

// Exercise 12: Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  override def toList[A](as: List[A]): List[A] =
    as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  override def toList[A](as: IndexedSeq[A]): List[A] =
    as.toList
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  override def toList[A](as: Stream[A]): List[A] =
    as.toList
}

import fpinscala.datastructures._
// Exercise 13: Recall the binary Tree data type from chapter 3. Implement a Foldable instance for it.
object TreeFoldable extends Foldable[Tree] {
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  override def toList[A](as: Tree[A]): List[A] =
    foldLeft(as)(List.empty[A])((as, a) => a :: as).reverse
}

// Exercise 14: Write a Foldable[Option] instance.
object OptionFoldable extends Foldable[Option] {
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None => z
      case Some(a) => f(z, a)
    }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case None => z
      case Some(a) => f(a, z)
    }
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }
}
