object state {
  trait RNG {
    def nextInt: (Int, RNG) 
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  object RNG {
    // Exercise 6.1: Write a function that uses RNG.nextInt to generate a random integer between 0 and
    // Int.MaxValue (inclusive).
    def nonNegativeInt(rng: RNG): (Int, RNG) =  {
      val (i, r) = rng.nextInt
      ((i % Int.MaxValue) + 1, r)
    }

    // Exercise 6.2: Write a function to generate a Double between 0 and 1 , not including 1
    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      ((i % Int.MaxValue).toDouble / Int.MaxValue, r)
    }

    // Exercise 6.3: Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
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

    // Exercise 6.4: Write a function to generate a list of random integers.
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
  }

}
