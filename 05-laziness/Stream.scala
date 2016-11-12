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
  }

}
