package fpinscala.applicative

import fpinscala.applicative.Applicative.streamApplicative
import org.scalatest.{FlatSpec, Matchers}

class TraversableSpec extends FlatSpec with Matchers {
  import Traverse._

  private implicit val SA: Applicative[Stream] = streamApplicative

  // Exercise 13, 14, 16, 17, 18, 19

  "the listTraverse" should "traverse a List" in {
    listTraverse.traverse[Stream, Int, Int](List(1, 2))(i => Stream(i, -i)) shouldBe Stream(List(1, 2), List(-1, -2))
  }

  it should "map a List" in {
    listTraverse.map(List(1, 2, 3))(_ * 10) shouldBe List(10, 20, 30)
  }

  it should "reverse a List" in {
    listTraverse.reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  it should "foldLeft a List" in {
    listTraverse.foldLeft(List(1, 2, 3))(0)(_ - _) shouldBe -6
  }

  it should "fuse two List traversals" in {
    listTraverse.fuse[Stream, Stream, Int, Int](List(1, 2))(i => Stream(i), i => Stream(-i)) shouldBe (Stream(List(1, 2)), Stream(List(-1, -2)))
  }
  "the optionTraverse" should "traverse an Option" in {
    optionTraverse.traverse[Stream, Int, String](Some(1))(i => Stream(i.toString, (-i).toString)) shouldBe Stream(Some("1"), Some("-1"))
  }

  it should "map an Option" in {
    optionTraverse.map(Some("a"))(_.toUpperCase) shouldBe Some("A")
    optionTraverse.map(Option.empty[String])(_.toUpperCase) shouldBe None
  }

  it should "reverse an Option ;)" in {
    optionTraverse.reverse(Some(1)) shouldBe Some(1)
    optionTraverse.reverse(None) shouldBe None
  }

  it should "foldLeft an Option" in {
    optionTraverse.foldLeft(Some(2))(10)(_ * _) shouldBe 20
    optionTraverse.foldLeft(Option.empty[Int])(10)(_ * _) shouldBe 10
  }

  it should "fuse two Option traversals" in {
    optionTraverse.fuse[Stream, Stream, Int, String](Some(1))(
      i => Stream(i.toString), i => Stream((-i).toString)) shouldBe (Stream(Some("1")), Stream(Some("-1")))
  }

  "the treeTraversable" should "traverse a Tree" in {
    treeTraverse.traverse[Stream, Int, Double](Tree(1, List(Tree(2, List.empty)))
    )(i => Stream(i / 2.0, i * 2.0)) shouldBe Stream(Tree(0.5, List(Tree(1.0, List.empty))), Tree(2.0, List(Tree(4.0, List.empty))))
  }

  it should "map a Tree" in {
    treeTraverse.map(Tree(1, List(Tree(2, List.empty))))(_ + 1) shouldBe Tree(2, List(Tree(3, List.empty)))
  }

  it should "reverse a Tree" in {
    treeTraverse.reverse(Tree(1, List(Tree(2, List.empty)))) shouldBe Tree(2, List(Tree(1, List.empty)))
  }

  it should "foldLeft a Tree" in {
    treeTraverse.foldLeft(Tree(1, List(Tree(2, List.empty))))(0)(_ - _) shouldBe -3
  }

  it should "fuse two Tree traversals" in {
    treeTraverse.fuse[Stream, Stream, Int, String](Tree(1, List(Tree(2, List.empty))))(
      i => Stream(i.toString), i => Stream((-i).toString)) shouldBe (
      Stream(Tree("1", List(Tree("2", List.empty)))),
      Stream(Tree("-1", List(Tree("-2", List.empty))))
    )
  }

  "A Traversal" should "compose" in {
    listTraverse.compose(optionTraverse)
      .traverse[Stream, Int, String](List(Some(1), Some(2)))(i => Stream(i.toString)) shouldBe Stream(List(Some("1"), Some("2")))
  }

}
