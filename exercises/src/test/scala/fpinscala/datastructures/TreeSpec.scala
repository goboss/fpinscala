package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  // Exercise 25

  "A Tree" should "calculate the size of Leaf as 1" in {
    val leaf = Leaf("A")
    Tree.size(leaf) shouldBe 1
    Tree.size2(leaf) shouldBe 1
  }

  it should "calculate the size of a tree with branches" in {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    Tree.size(tree) shouldBe 5
    Tree.size2(tree) shouldBe 5
  }

  // Exercise 26

  it should "find maximum element in a Leaf" in {
    val leaf = Leaf(1)
    Tree.maximum(leaf) shouldBe 1
    Tree.maximum2(leaf) shouldBe 1
  }

  it should "find maximum element in a tree with branches" in {
    val tree = Branch(Leaf(5), Branch(Leaf(2), Leaf(3)))
    Tree.maximum(tree) shouldBe 5
    Tree.maximum2(tree) shouldBe 5
  }

  // Exercise 27

  it should "calculate the depth of Leaf as 0" in {
    val leaf = Leaf("just me")
    Tree.depth(leaf) shouldBe 0
    Tree.depth2(leaf) shouldBe 0
  }

  it should "calculate the depth of a tree with branches" in {
    val prettyUnbalanced = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
    Tree.depth(prettyUnbalanced) shouldBe 3
    Tree.depth2(prettyUnbalanced) shouldBe 3
  }

  // Exercise 28

  it should "map elements of a tree" in {
    val powerTree = Branch(Leaf("I"), Branch(Leaf("haz"), Leaf("a cat")))
    Tree.map(powerTree)(_.length) shouldBe Branch(Leaf(1), Branch(Leaf(3), Leaf(5)))
    Tree.map2(powerTree)(_.length) shouldBe Branch(Leaf(1), Branch(Leaf(3), Leaf(5)))
  }

  // Exercise 29

  it should "fold a Leaft into a value" in {
    Tree.fold(Leaf(2))(_ / 2)((_, _) => fail()) shouldBe 1
  }

  it should "fold a tree into a value" in {
    Tree.fold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ + 1)(_ - _) shouldBe 3
  }

}
