object datastructures {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List { // `List` companion object. Contains functions for creating and working with lists.
    def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


    // Exercise 3.2: implement tail in constant time
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("tail of an empty list")
      case Cons(_, t) => t
    }

    // Exercise 3.3: replace the head on list
    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => Cons(h, Nil)
      case Cons(_, t) => Cons(h, t)
    }

    // Exercise 3.4: generalize tail to function that removes the first n elements from a list
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(_, t) if n > 0 => drop(t, n-1)
      case other => other
    }

    // Exercie 3.5: remove elements from a list as long as they match a predicate
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case other => other
    }

    // Exercise 3.6: implement the sad function init that returns all but the last element of a list
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    // Exercise 3.9: compute the length of a list using foldRight
    def length[A](l: List[A]): Int =
      foldRight(l, 0)((_, acc) => acc + 1)

    // Exercise 3.10: implement a stack-safe foldLeft
    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    // Exercie 3.11: write sum, product and length using foldLeft
    def sum3(l: List[Int]): Int =
      foldLeft(l, 0)(_ + _)

    def product3(l: List[Int]): Int = 
      foldLeft(l, 1)(_ * _)

    def length2[A](l: List[A]): Int = 
      foldLeft(l, 0)((acc, _) => acc + 1)

    // Exercise 3.12: write a function that returns the reverse of a list using a fold
    def reverse[A](l: List[A]): List[A] = 
      foldLeft(l, List[A]())((as, a) => Cons(a, as))

    // Exercie 3.13: write foldLeft in terms of foldRight and vice versa
    def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = 
      foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = 
      foldLeft(reverse(as), z)((b, a) => f(a, b))

    // Exercise 3.14: implement append using a fold
    def append2[A](l1: List[A], l2: List[A]): List[A] = 
      foldRight(l1, l2)(Cons(_, _))

    // Exercise 3.15: write a function that concatenates a list of lists into a single list
    def flatten[A](l: List[List[A]]): List[A] = 
      foldLeft(l, List[A]())(append)
  }
}