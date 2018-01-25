package fpinscala.localeffects

import org.scalatest.{FlatSpec, Matchers}

class LocalEffectsSpec extends FlatSpec with Matchers {
  // Exercise 1

  "STArray" should "fill array from a map with indexes as keys" in {
    ST.runST(new RunnableST[List[Char]] {
      override def apply[S]: ST[S, List[Char]] =
        for {
          ar <- STArray(3, 'a')
          _ <- ar.fill(Map(0 -> 'x', 1 -> 'y', 2 -> 'z'))
          r <- ar.freeze
        } yield r
    }) shouldBe List('x', 'y', 'z')
  }

  // Exercise 2

  "Immutable" should "implement purely functional partition" in {
    ST.runST(new RunnableST[Int] {
      override def apply[S]: ST[S, Int] =
        for {
          a <- STArray.fromList(List(0, 1, 1, 2, 4, 5))
          p <- Immutable.partition(a, 1, 4, 3)
        } yield p
    }) shouldBe 3
  }

  it should "implement purely functional quicksort" in {
    ST.runST(new RunnableST[List[Int]] {
      override def apply[S]: ST[S, List[Int]] =
        for {
          a <- STArray.fromList(List(4, 1, 2, 6, 8, 1, 0))
          _ <- Immutable.qs(a, 1, 5)
          s <- a.freeze
        } yield s
    }) shouldBe List(4, 1, 1, 2, 6, 8, 0)
  }

  // Exercise 3

  "STHashMap" should "be fun to work with" in {
    val (d1, d2, s, lst) =
      ST.runST(new RunnableST[(Boolean, Boolean, Int, List[(String, Int)])] {
        override def apply[S]
          : ST[S, (Boolean, Boolean, Int, List[(String, Int)])] =
          for {
            hm <- STHashMap.fromList(List("x" -> 0))
            _ <- hm += ("x", 1)
            _ <- hm += ("x", 2)
            _ <- hm += ("y", 3)
            _ <- hm += ("z", 4)
            d1 <- hm.isDefinedAt("z")
            _ <- hm -= "z"
            d2 <- hm.isDefinedAt("z")
            s <- hm.size
            r <- hm.freeze
          } yield (d1, d2, s, r)
      })

    d1 shouldBe true
    d2 shouldBe false
    s shouldBe 2
    lst should contain theSameElementsAs List("x" -> 2, "y" -> 3)
  }
}
