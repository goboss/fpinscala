package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {
  "A List" should "calculate the length of Nil as 0" in {
    List.length(Nil) shouldBe 0
  }

  it should "calculate the length of Cons with Nil tail as 1" in {
    List.length(Cons("first!", Nil)) shouldBe 1
  }

  it should "calculate the length of List constructed from varargs correctly" in {
    List.length(List(1, 2)) shouldBe 2
  }

  it should "calculate the length of List constructed by appending 2 lists as the sum of their lengths" in {
    List.length(List.append(List(1, 2), List(3))) shouldBe 3
  }

  it should "fail to get the tail of Nil" in {
    a[RuntimeException] shouldBe thrownBy(List.tail(Nil))
  }

  it should "get the tail of a non empty list" in {
    List.tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  it should "get Nil as the tail of a single element list" in {
    List.tail(List(true)) shouldBe Nil
  }

  it should "set the head of a non empty list" in {
    List.setHead(List(1, 2, 3), 0) shouldBe List(0, 2, 3)
  }

  it should "set the head of an empty list" in {
    List.setHead(Nil, 1) shouldBe List(1)
  }

  it should "subtract elements of a list starting from left" in {
    List.foldLeft(List(1, 2, 3), 0)(_ - _) shouldBe -6
  }

  it should "subtract elements of a list starting from the right" in {
    List.foldRight(List(1, 2, 3), 0)(_ - _) shouldBe 2
  }

  it should "get zero element from foldLeft on Nil" in {
    List.foldLeft(Nil, 42)((_, _) => fail()) shouldBe 42
  }

  it should "get zero element from foldRight on Nil" in {
    List.foldRight(Nil, 42)((_, _) => fail()) shouldBe 42
  }
}
