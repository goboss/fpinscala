import scala.annotation._

object GettingStarted {

  // Exercise 2.1: recursive def of nth Fibonacci number
  def fib(n: Int): Int = {
    @tailrec
    def go(step: Int, i: Int, j: Int): Int = 
      if (step == n) i
      else go(step + 1, j, i + j)

    require(n >= 0)
    go(0, 0, 1)
  }

  // Exercise 2.2: check whether an Array[A] is sorted according to a given predicate
  // It seems I can assume that the provided relation is transitive :)
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean = 
      if (i >= as.length - 1) true
      else ordered(as(i), as(i+1)) && go(i+1) // && is lazy

    go(0)
  }

  // Exercise 2.3: implement curring :)
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 2.4: implement uncurry
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5: implement function composition
  def compose[A, B, C](f: B => C, g: A => B): A => C = 
    a => f(g(a))
}

