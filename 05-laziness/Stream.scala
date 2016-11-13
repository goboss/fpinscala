object laziness {

  sealed trait Stream[+A] {
		def headOption: Option[A] = this match {
			case Empty => None
			case Cons(h, t) => Some(h())
		}

		// Exercise 5.1: write a function to convert a Stream to a List
		def toList: List[A] = {
			def loop(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(h, t) => loop(t(), h() :: acc)
			}

      loop(this, Nil).reverse
		}

    // Exercise 5.2: write a function take(n) for returning the first n elements of a Stream, and
    // drop(n) for skipping the first n elements of a Stream
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => t().drop(n-1)
      case _ => this
    }

    // Exercise 5.3: write the function takeWhile for returning all starting elements of a Stream
    // that match the given predicate
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

		// Exercise 5.4: implement forAll, which checks that all elements in the Stream match a predicate.
    // Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
    def forAll(p: A => Boolean): Boolean = 
      foldRight(true)((a, t) => p(a) && t)

    // Exercise 5.5: use foldRight to implement takeWhile
    def takeWhile2(p: A => Boolean): Stream[A] = 
      foldRight(Stream.empty[A])((a, t) => if(p(a)) Stream.cons(a, t) else Stream.empty)

    // Exercise 5.6: implement headOption using foldRight
    def headOption2: Option[A] = 
      foldRight(None: Option[A])((a, t) => Some(a))

    // Exercise 5.7: implement map, filter, append and flatMap using foldRight. The append method
    // should be non-strict in its argument.
    def map[B](f: A => B): Stream[B] = 
      foldRight(Stream.empty[B])((a, t) => Stream.cons(f(a), t))

    def filter(p: A => Boolean): Stream[A] = 
      foldRight(Stream.empty[A])((a, t) => if(p(a)) Stream.cons(a, t) else t)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((a, t) => Stream.cons(a, t))

    // Exercise 5.14: Implement statsWith using function you've written
    def startsWith[A](prefix: Stream[A]): Boolean = 
      Stream.zipAll(this, prefix).takeWhile(_._2.nonEmpty).forAll {
        case (h1, h2) => h1 == h2
      }

    // Exercise 5.15: Implement tails using unfold
    def tails: Stream[Stream[A]] = Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(Stream(Stream.empty))

    def hasSubsequence[A](s: Stream[A]): Boolean =
      tails exists (_ startsWith s)

    // Exercise 5.16: Generalize tails to the function scanRight , which is like a foldRight that
    // returns a stream of the intermediate results.
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z))) {
        case (a, (s0, s)) =>
          lazy val v = f(a, s0)
          (v, Stream.cons(v, s))
      }._2
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
		def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
			lazy val head = hd
			lazy val tail = tl
			Cons(() => head, () => tail)
		}
  
		def empty[A]: Stream[A] = Empty

		def apply[A](as: A*): Stream[A] =
    	if (as.isEmpty) empty
    	else cons(as.head, apply(as.tail: _*))

    // Exercise 5.8: Generalize ones slightly to the function constant , which returns an infinite Stream of
    // a given value.
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // Exercise 5.9: Write a function that generates an infinite stream of integers.
    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    // Exercise 5.10: Write a function fibs that generates the infinite stream of Fibonacci numbers.
    def fibs: Stream[Int] = {
      def go(a: Int, b: Int): Stream[Int] = 
        cons(a, go(b, a + b))

      go(0, 1)
    }

    // Exercise 5.11: Write a more general stream-building function called unfold
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
      f(z) match {
        case None => Empty
        case Some((a, s)) => cons(a, unfold(s)(f))
      }

    // Exercise 5.12: Write fibs , from , constant , and ones in terms of unfold
    def fibs2: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, a+b))) }

    def from2(n: Int): Stream[Int] = unfold(n)(i => Some((i, i + 1)))

    def constant2[A](a: A): Stream[A] = unfold(a)(s => Some((a, a)))

    def ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))

    // Exercise 5.13: Use unfold to implement map , take , takeWhile , zipWith (as in chapter 3), and
    // zipAll
    def map[A, B](s: Stream[A])(f: A => B): Stream[B] = unfold(s) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

    def take[A](s: Stream[A], n: Int): Stream[A] = unfold((s, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

    def takeWhile[A](s: Stream[A])(p: A => Boolean) = unfold(s) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

    def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] = 
      unfold((as, bs)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2())), (t1(), t2()))
        case _ => None
      }

    def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A],Option[B])] = 
      unfold((s1, s2)) {
        case (Empty, Empty) => None
        case (as, bs) => Some(((as.headOption, bs.headOption), (as.drop(1), bs.drop(1))))
      }
  }

}
