package fpinscala.errorhandling

import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // Exercise 1: Implement basic functions on Option
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some.apply).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(v => if(f(v)) Some(v) else None)

  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
}
case class Some[+A](get: A) extends Option[A] {
  override def isEmpty: Boolean = false
}
case object None extends Option[Nothing] {
  override def isEmpty: Boolean = true
}

object Option {
  def empty[A]: Option[A] = None

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 2: Implement variance function in terms of flatMap
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // Exercise 3: Write a generic function map2 that combines two Option values using a binary function
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(va => b.map(vb => f(va, vb)))

  // Exercise 4: write a function that combines a list of Options into an Option of list
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List.empty[A]): Option[List[A]])((o, acc) => acc.flatMap(a => o.map(v => v +: a)))

  // Exercise 5: Write a function traverse and implement sequence using it
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List.empty[B]): Option[List[B]])((a, acc) =>
      for {
        t <- acc
        b <- f(a)
      } yield b +: t
    )

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}
