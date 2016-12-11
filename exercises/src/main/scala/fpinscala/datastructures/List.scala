package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
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


  // Exercise 2: Implement tail in constant time
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of an empty list")
    case Cons(_, t) => t
  }

  // Exercise 3: Replace the head on list
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 4: Generalize tail to function that removes the first n elements from a list
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n-1)
    case _ => l
  }

  // Exercise 5: Remove elements from a list as long as they match a predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // Exercise 6: Implement the function init that returns all but the last element of a list
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t)) // not stack safe, but good looking ;)
  }

  // Exercise 9: Compute the length of a list using foldRight
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  // Exercise 10: implement a stack-safe foldLeft
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 11: Write sum, product and length using foldLeft
  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]): Int =
    foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  // Exercise 12: Write a function that returns the reverse of a list using a fold
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((as, a) => Cons(a, as))

  // Exercise 13: Write foldLeft in terms of foldRight and vice versa
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  // Exercise 14: Implement append using a fold
  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight2(l1, l2)(Cons(_, _))

  // Exercise 15: Write a function that concatenates a list of lists into a single list
  def flatten[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())(append)

  // Exercise 16: Add 1 to each element of list of ints
  def inc(is: List[Int]): List[Int] =
    foldRight2(is, Nil: List[Int])((i, acc) => Cons(i+1, acc))

  // Exercise 17: Write a function that turns each Double into a String
  def d2s(ds: List[Double]): List[String] =
    foldRight2(ds, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  // Exercise 18: Write a map finally!
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight2(l, Nil: List[B])((a, acc) => Cons(f(a), acc))

  // Exercise 19: Write a function filter
  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    foldRight2(as, Nil: List[A])((a, acc) => if(p(a)) Cons(a, acc) else acc)

  // Exercise 20: Write flatMap
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  // Exercise 21: Use flatMap to implement filter
  def filter2[A](as: List[A])(p: A => Boolean): List[A] =
    flatMap(as)(a => if (p(a)) List(a) else Nil)

  // Exercise 22: Write a function that accepts two lists and constructs a new list by adding corresponding elements
  def addTogether(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(a + b, addTogether(ta, tb)) // not stack safe!
  }

  // Exercise 23: Generalize addTogether
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(f(a, b), zipWith(ta, tb)(f)) // not stack safe!
  }

  // Exercise 24: implement a function for checking whether a list contains another list as a subsequence
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasPrefix(l: List[A], p: List[A]): Boolean = (l, p) match {
      case (_, Nil) =>
        true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 =>
        hasPrefix(t1, t2)
      case _ =>
        false
    }

    sup match {
      case Nil =>
        sub == Nil
      case l if hasPrefix(l, sub) =>
        true
      case Cons(_, t) =>
        hasSubsequence(t, sub)
    }
  }
}
