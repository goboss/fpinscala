package fpinscala.parsing

import org.scalatest.{FlatSpec, Matchers}

class ParsersSpec extends FlatSpec with Matchers {
  import MyParsers._
  import MyParsers.operators

  "Parsers" should "parse a char" in {
    MyParsers.run(char('a'))("aba") shouldBe Success('a', Location("aba", 1))
  }

  it should "fail to parse a char" in {
    MyParsers.run(char('a'))("ba") shouldBe Failure(ParseError(List(Location("ba", 0) -> "expected string 'a'")))
  }

  it should "parse a string" in {
    MyParsers.run(string("java"))("javascript") shouldBe Success("java", Location("javascript", 4))
  }

  it should "fail to parse a string" in {
    MyParsers.run(string("java"))("jax") shouldBe Failure(ParseError(List(Location("jax", 0) -> "expected string 'java'")))
  }

  it should "parse regex" in {
    MyParsers.run(MyParsers.regex("[0-9]+".r))("123") shouldBe Success("123", Location("123", 3))
  }

  it should "fail to parse regex" in {
    MyParsers.run(MyParsers.regex("[0-9]+".r))("a123") shouldBe Failure(ParseError(List(Location("a123", 0) -> "expected regex '[0-9]+'")))
  }

  it should "parse one of two things" in {
    MyParsers.run(digit | space)("1") shouldBe Success("1", Location("1", 1))
    MyParsers.run(digit | space)(" ") shouldBe Success(" ", Location(" ", 1))
  }

  it should "fail to parse one of two things" in {
    MyParsers.run(digit | space)("doh") shouldBe Failure(
      ParseError(List(Location("doh", 0) -> "expected regex '\\s+'"), List(ParseError(List(Location("doh", 0) -> "expected regex '\\d'"))))
    )
  }

  // Exercise 1

  it should "map 2 parsers" in {
    MyParsers.run(map2(MyParsers.succeed(1), MyParsers.succeed("one"))(_ -> _))("") shouldBe Success(1 -> "one", Location("", 0))
  }

  it should "parse at least one element" in {
    MyParsers.run(many1(digit))("456") shouldBe Success(List("4", "5", "6"), Location("456", 3))
  }

  it should "fail to parse at least one element" in {
    MyParsers.run(many1(digit))("aaa") shouldBe Failure(ParseError(List(Location("aaa", 0) -> "expected regex '\\d'")))
  }

  // Exercise 3

  it should "parse a list of elements" in {
    MyParsers.run(many(digit))("456") shouldBe Success(List("4", "5", "6"), Location("456", 3))
    MyParsers.run(many(digit))("aaa") shouldBe Success(List.empty[String], Location("aaa", 0))
  }

  // Exercise 4

  it should "parse exactly n elements" in {
    MyParsers.run(listOfN(3, digit))("456") shouldBe Success(List("4", "5", "6"), Location("456", 3))
  }

  it should "fail to parse exactly n elements" in {
    MyParsers.run(listOfN(4, digit))("456") shouldBe Failure(ParseError(List(Location("456", 3) -> "expected regex '\\d'")))
  }

  // Exercise 6

  it should "parse a number of 'a' chars specified in the first digit" in {
    MyParsers.run(numberOfAs)("3aaa") shouldBe Success(List('a', 'a', 'a'), Location("3aaa", 4))
  }

  it should "fail to parse a number of 'a' chars specified in the first digit" in {
    MyParsers.run(numberOfAs)("3aa") shouldBe Failure(ParseError(List(Location("3aa", 3) -> "expected string 'a'")))
  }

  // Exercise 7

  it should "parse a product of 2 parsers" in {
    MyParsers.run(product(digit, char('a')))("3a") shouldBe Success(("3", 'a'), Location("3a", 2))
  }

  // Exercise 8

  it should "map the result of parser" in {
    MyParsers.run(map(digit)(_.toInt))("4") shouldBe Success(4, Location("4", 1))
  }

}
