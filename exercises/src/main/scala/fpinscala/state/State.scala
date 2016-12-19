package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  val boolean: Rand[Boolean] = map(int)(_ % 2 == 0)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, r1) = s(rng)
      (f(a), r1)
    }

  // Exercise 1: Write a function that uses RNG.nextInt to generate a random integer between 0 and
  // Int.MaxValue (inclusive).
  def nonNegativeInt(rng: RNG): (Int, RNG) =  {
    val (i, r) = rng.nextInt
    (if (i < 0) -i +1 else i, r)
  }

  // Exercise 2: Write a function to generate a Double between 0 and 1 , not including 1
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    ((i % Int.MaxValue).toDouble / Int.MaxValue, r)
  }

  // Exercise 3: Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
  // (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
  // already written.
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  // Exercise 4: Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(c: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (c > 0) {
        val (i, r) = rng.nextInt
        go(c-1, r, i :: acc)
      }
      else
        (acc, rng)
    }

    go(count, rng, List.empty)
  }

  // Exercise 5: Use map to reimplement double in a more elegant way
  val double2: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6: Write the implementation of map2
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    r0 => {
      val (a, r1) = ra(r0)
      val (b, r2) = rb(r1)
      (f(a, b), r1)
    }

  // Exercise 7: Implement sequence for combining a List of transitions into a single
  // transition.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  rng => {
    val (finalAs, finalR) = fs.foldLeft((List.empty[A], rng)) {
      case ((as, r), ra) =>
        val (a, rNext) = ra(r)
        (a :: as, rNext)
    }

    // does it really matter that the values are reversed?
    (finalAs.reverse, finalR)
  }

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(nonNegativeInt))

  // Exercise 8: Implement flatMap , and then use it to implement nonNegativeLessThan.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  // Exercise 9: Reimplement map and map2 in terms of flatMap
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }
}

// Exercise 10: Generalize the functions unit , map , map2 , flatMap , and sequence . Add them as methods on the State case class where possible.
// Otherwise you should put them in a State companion object.
case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](List.empty)) { (s, acc) =>
      s.map2(acc)(_ :: _)
    }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Exercise 11: To gain experience with the use of State, implement a finite state automaton that models a simple candy dispenser.
  // The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
  // It can be in one of two states: locked or unlocked. It also tracks how many candies are left and how many coins it contains.
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def input(i: Input)(m: Machine): Machine =
    (m, i) match {
      case (Machine(_, 0, _), _) => m
      case (Machine(true, _, _), Turn) => m
      case (Machine(false, _, _), Coin) => m
      case (Machine(true, ca, co), Coin) => Machine(locked = false, ca, co + 1)
      case (Machine(false, ca, co), Turn) => Machine(locked = true, ca - 1, co)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(inputs.map(i => State.modify(input(i))))
      s <- State.get
    } yield (s.candies, s.coins)
}
