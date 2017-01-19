package fpinscala.parsing

import fpinscala.parsing.MyParser.MyParser
import fpinscala.testing.{Gen, Prop}
import fpinscala.testing.Prop.forAll

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  // TODO: document these methods
  def run[A](p: Parser[A])(input: String): ParseResult[A]
  def succeed[A](a: A): Parser[A]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def slice[A](p: Parser[A]): Parser[String]

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  // Exercise 10: Spend some time discovering a nice set of combinators for expressing what errors get reported by a Parser
  def describe[A](p: Parser[A], description: String): Parser[A]
  def important[A](p: Parser[A]): Parser[A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  // Exercise 8: map is no longer primitive. Express it in terms of flatMap and/or other combinators.
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  // Exercise 1: Using product, implement the now-familiar combinator map2 and then use this to implement many1 in terms of many.
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p1)(a => map(p2)(b => f(a, b)))

  // Exercise 7: Implement product and map2 in terms of flatMap.
  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p1)(a => map(p2)(b => (a, b)))

  // Exercise 3: Before continuing, see if you can define many in terms of or, map2, and succeed.
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List.empty)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // Exercise 4: Using map2 and succeed, implement the listOfN combinator from earlier.
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    @tailrec
    def go(k: Int, acc: Parser[List[A]]): Parser[List[A]] = {
      if (k > 0) go(k - 1, map2(p, acc)(_ :: _))
      else map(acc)(_.reverse)
    }

    go(n, succeed(List.empty))
  }

  // Exercise 5: We could also deal with non-strictness with a separate combinator like we did in chapter 7.
  // Try this here and make the necessary changes to your existing combinators.
  // What do you think of that approach in this instance?
  def lick[A](p: Parser[A]): Boolean
  def manyLicks[A](p: Parser[A]): Parser[List[A]] = {
    @tailrec
    def go(acc: Parser[List[A]]): Parser[List[A]] = {
      if(lick(p)) go(map2(p, acc)(_ :: _))
      else acc.map(_.reverse)
    }

    go(succeed(List.empty))
  }

  def digit: Parser[String] =
    "\\d".r

  def space: Parser[String] =
    "\\s*".r

  def double: Parser[Double] =
    regex("""-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r).map(_.toDouble)

  def literal[A](lit: String)(a: A): Parser[A] =
    slice(lit).map(_ => a)

  def trimLeft[L, A](left: Parser[L], p: Parser[A]): Parser[A] =
    map2(slice(many(left)), p)((_, a) => a)

  def trimRight[A, R](p: Parser[A], right: Parser[R]): Parser[A] =
    map2(p, slice(many(right)))((a, _) => a)

  def trim[L, A, R](left: Parser[L], p: Parser[A], right: Parser[R]): Parser[A] =
    for {
      _ <- slice(left)
      a <- p
      _ <- slice(right)
    } yield a

  def trimSpace[A](p: Parser[A]): Parser[A] =
    trim(space, p, space)

  def manySep[S, A](s: Parser[S], p: Parser[A]): Parser[List[A]] =
    map2(p, many(trimLeft(s, p)))(_ :: _)

  // Exercise 6: Using flatMap and any other combinators, write the context-sensitive parser we couldn’t express earlier.
  def numberOfAs: Parser[List[Char]] =
    digit.flatMap(s => listOfN(s.toInt, char('a')))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = or(p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def ~>[B](p2: => Parser[B]): Parser[B] = self.trimLeft(p, p2)
    def <~[B](p2: => Parser[B]): Parser[A] = self.trimRight(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A,B)] = product(p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)

    def describedAs(description: String): Parser[A] = self.describe(p, description)

    def important: Parser[A] = self.important(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    // Exercise 2: Try coming up with laws to specify the behavior of product.
    def productLaw[A, B](p: Parser[A])(in: Gen[String]): Prop =
      equal(p ** p, p.map(a => (a, a)))(in)
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

  def advanceBy(s: String): Location = if(input.startsWith(s)) advanceBy(s.length) else this

  def currentPos: String = input.drop(offset)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

// Exercise 18: Change the representation of ParseError to keep track of errors that occurred in other branches of the parser.
case class ParseError(
  stack: List[(Location, String)] = List.empty,
  otherFailures: List[ParseError] = List.empty
) {

  // Exercise 16: Come up with a nice way of formatting a ParseError for human consumption.
  def show: String = {
    stack
      .groupBy { case (loc, _) => loc }
      .map { case (loc, errors) =>
        val errorDesc = errors.map(_._2).mkString("\n")

        s"Error at ${loc.line}:${loc.col}: $errorDesc\nnear: ${loc.currentLine}" + (
          if(otherFailures.nonEmpty) s"\n\nOther problems:\n" + otherFailures.map(_.show).mkString("\n")
          else ""
        )
      }
      .mkString("\n")
  }

  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def pushOther(error: ParseError): ParseError =
    copy(otherFailures = error +: otherFailures)
}

trait ParseResult[+A] {
  def mapError(f: ParseError => ParseError): ParseResult[A] = this match {
    case Failure(e, c) => Failure(f(e), c)
    case _ => this
  }

  def commit: ParseResult[A] = this match {
    case Failure(e, false) => Failure(e, isCommited = true)
    case _ => this
  }
}
case class Success[+A](value: A, ahead: Location) extends ParseResult[A] {
  def advanceSuccess(n: Int): ParseResult[A] = this match {
    case Success(a, m) => Success(a, m.advanceBy(n))
    case _ => this
  }
}
case class Failure(error: ParseError, isCommited: Boolean = false) extends ParseResult[Nothing] {
  def addCommit(isCommitted: Boolean): ParseResult[Nothing] = this match {
    case Failure(e,c) => Failure(e, c || isCommitted)
    case _ => this
  }

  override def toString: String = error.show
}

object MyParser {
  type MyParser[+A] = ((Location) => ParseResult[A])
}

object MyParsers extends Parsers[MyParser] {
  def consume(loc: Location, s: String): ParseResult[String] = Success(s, loc.advanceBy(s.length))

  override def run[A](p: MyParser[A])(input: String): ParseResult[A] = p(Location(input))

  // Exercise 13: Implement string, regex, succeed, and slice for this initial representation of Parser.
  // Exercise 14: Revise your implementation of string to use scope and/or label to provide a meaningful error message in the event of an error.
  override implicit def string(s: String): MyParser[String] = loc =>
    if(loc.currentPos.startsWith(s))
      consume(loc, s)
    else
      Failure(loc.toError(s"expected string $s"))

  override implicit def regex(r: Regex): MyParser[String] = loc =>
    r.findPrefixOf(loc.currentPos) match {
      case Some(s) => consume(loc, s)
      case _ => Failure(loc.toError(s"expected regex $r"))
    }

  override def succeed[A](a: A): MyParser[A] = loc =>
    Success(a, loc)

  // Exercise 17: Think of a way of modifying the Parser representation to make slicing more efficient.
  // One way to achieve this is to switch parsers into "lookahead" mode (via some flag or param) in which they do not
  // consume input, but simply try matching and returning the longest prefix.
  // This would require modifying primitive operations to act accordingly depending on the flag.
  override def slice[A](p: MyParser[A]): MyParser[String] = loc =>
    p(loc) match {
      case Success(_, ahead) => Success(loc.input.substring(loc.offset, ahead.offset), ahead)
      case Failure(error, c) => Failure(error, c)
    }

  override def or[A](p1: MyParser[A], p2: => MyParser[A]): MyParser[A] = loc =>
    p1(loc) match {
      case Failure(e, false) => p2(loc).mapError(_.pushOther(e))
      case r => r
    }

  // Exercise 15: Implement the rest of the primitives, including run, using this representation of Parser, and try running your JSON parser on various inputs.
  override def flatMap[A, B](p: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] = loc =>
    p(loc) match {
      case Success(v, ahead) => f(v)(ahead)
      case Failure(e, c) => Failure(e, c)
    }

  override def describe[A](p: MyParser[A], description: String): MyParser[A] = loc =>
    p(loc).mapError(_.push(loc, description))

  override def important[A](p: MyParser[A]): MyParser[A] = loc =>
    p(loc).commit

  override def lick[A](p: MyParser[A]): Boolean = ???
}
