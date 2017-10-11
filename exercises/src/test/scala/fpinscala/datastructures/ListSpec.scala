package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {
  // Exercise 2

  "A List" should "fail to get the tail of Nil" in {
    a[RuntimeException] shouldBe thrownBy(List.tail(Nil))
  }

  it should "get the tail of a non empty list" in {
    List.tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  it should "get Nil as the tail of a single element list" in {
    List.tail(List(true)) shouldBe Nil
  }

  // Exercise 3

  it should "set the head of a non empty list" in {
    List.setHead(List(1, 2, 3), 0) shouldBe List(0, 2, 3)
  }

  it should "set the head of Nil" in {
    List.setHead(Nil, 1) shouldBe List(1)
  }

  // Exercise 4

  it should "drop n elements from Nil" in {
    List.drop(Nil, 3) shouldBe Nil
  }

  it should "drop 1 element from a non empty list" in {
    List.drop(List(1, 2, 3), 2) shouldBe List(3)
  }

  it should "drop n elements from a list with n - 1 elements in it" in {
    List.drop(List(1, 2, 3), 4) shouldBe Nil
  }

  // Exercise 5

  it should "drop elements from Nil while the provided predicate keeps returning true" in {
    List.dropWhile(Nil, (_: Int) => true) shouldBe Nil
  }

  it should "drop all elements from non empty list while the provided predicate keeps returning true" in {
    List.dropWhile(List(1, 2, 3), (_: Int) => true) shouldBe Nil
  }

  it should "drop elements from a list of integers while they are even" in {
    List.dropWhile(List(2, 4, 5, 6), (i: Int) => i % 2 == 0) shouldBe List(5, 6)
  }

  // Exercise 6

  it should "get Nil as the init of Nil" in {
    List.init(Nil) shouldBe Nil
  }

  it should "get the init of a non empty list" in {
    List.init(List(1, 2, 3)) shouldBe List(1, 2)
  }

  // Exercise 9

  it should "calculate the length of Nil as 0" in {
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

  // Exercise 10

  it should "subtract elements of a list starting from left" in {
    List.foldLeft(List(1, 2, 3), 0)(_ - _) shouldBe -6
  }

  it should "get zero element from foldLeft on Nil" in {
    List.foldLeft(Nil, 42)((_, _) => fail()) shouldBe 42
  }

  // Exercise 11

  it should "calculate sum, product and length using foldLeft" in {
    List.sum3(List(1, 2, 3)) shouldBe 6
    List.product3(List(2, 4, -1)) shouldBe -8
    List.length2(Nil) shouldBe 0
    List.length2(List(1, 2, 3)) shouldBe 3
  }

  // Exercise 12

  it should "reverse Nil" in {
    List.reverse(Nil) shouldBe Nil
  }

  it should "reverse a non empty list" in {
    List.reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  // Exercise 13

  it should "left fold elements of Nil using foldRight" in {
    List.foldLeft2(Nil, true)((_, _) => false) shouldBe true
  }

  it should "left fold elements of a non empty list using foldRight" in {
    List.foldLeft2(List(1, 2, 3), 0)(_ - _) shouldBe -6
  }

  it should "right fold elements of Nil using foldLeft" in {
    List.foldRight2(Nil, true)((_, _) => false) shouldBe true
  }

  it should "right fold elements of a non empty list using foldLeft" in {
    List.foldRight2(List(1, 2, 3), 0)(_ - _) shouldBe 2
  }

  // Exercise 14

  it should "append Nil and non empty list using a fold" in {
    List.append2(Nil, List(1, 2)) shouldBe List(1, 2)
  }

  it should "append non empty list and Nil using a fold" in {
    List.append2(List(1, 2), Nil) shouldBe List(1, 2)
  }

  it should "append two non empty lists using a fold" in {
    List.append2(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
  }

  // Exercise 15

  it should "flatten Nil" in {
    List.flatten(Nil) shouldBe Nil
  }

  it should "flatten a list of lists" in {
    List.flatten(List(List(1), List(2))) shouldBe List(1, 2)
  }

  // Exercise 16

  it should "add 1 to each element of a list of integers" in {
    List.inc(List(1, 3, 5)) shouldBe List(2, 4, 6)
  }

  // Exercise 17

  it should "map every element of a list of doubles to string" in {
    List.d2s(List(0.5d, 0.4d, 0.3d)) shouldBe List("0.5", "0.4", "0.3")
  }

  // Exercise 18

  it should "map Nil" in {
    List.map(Nil)((_: Int) => fail()) shouldBe Nil
  }

  it should "map a non empty list" in {
    List.map(List(1, 2))(i => "doubled: " + (i * 2)) shouldBe List("doubled: 2", "doubled: 4")
  }

  // Exercise 19

  it should "filter elements of Nil" in {
    List.filter(Nil)((_: Int) => true) shouldBe Nil
  }

  it should "filter elements of a non empty list" in {
    List.filter(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
  }

  // Exercise 20

  it should "flatMap Nil by applying the function 0 times" in {
    List.flatMap(Nil)((_: Int) => fail()) shouldBe Nil
  }

  it should "repeat every element of a non empty list" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  it should "drain the list of all elements" in {
    List.flatMap(List('a', 'b', 'c'))(_ => Nil) shouldBe Nil
  }

  // Exercise 21

  it should "filter elements of Nil using flatMap" in {
    List.filter2(Nil)((_: Int) => true) shouldBe Nil
  }

  it should "filter elements of a non empty list using flatMap" in {
    List.filter2(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
  }

  // Exercise 22

  it should "add corresponding elements from 2 lists, one of which is Nil" in {
    List.addTogether(List(1, 2), Nil) shouldBe Nil
    List.addTogether(Nil, List(1, 2)) shouldBe Nil
  }

  it should "add corresponding elements from 2 provided, non empty lists" in {
    List.addTogether(List(1, 2, 3), List(10, 20)) shouldBe List(11, 22)
  }

  // Exercise 23

  it should "zip two lists, one of which is Nil, together" in {
    List.zipWith(List(1, 2), Nil)((_, _) => fail()) shouldBe Nil
    List.zipWith(Nil, List(1, 2))((_, _) => fail()) shouldBe Nil
  }

  it should "zip two non empty lists together" in {
    List.zipWith(List(1, 2), List("dog", "cats"))((e1, e2) => e1 + " " + e2) shouldBe List("1 dog", "2 cats")
  }

  // Exercise 24

  it should "find subsequence of a list where there is one" in {
    List.hasSubsequence(List(1, 2, 3), List(1, 2)) shouldBe true
  }

  it should "find empty subsequence in a non empty list" in {
    List.hasSubsequence(List(1, 2), Nil) shouldBe true
  }

  it should "find empty subsequence in Nil" in {
    List.hasSubsequence(Nil, Nil) shouldBe true
  }

  it should "not find subsequence of a list where there is none" in {
    List.hasSubsequence(List(1, 2), List(2, 3)) shouldBe false
  }

  it should "not find non empty subsequence of Nil" in {
    List.hasSubsequence(Nil, List(1)) shouldBe false
  }
}
