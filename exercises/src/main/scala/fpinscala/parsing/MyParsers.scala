package fpinscala.parsing

import scala.util.matching.Regex
import MyParser._

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
      Failure(loc.toError(s"expected string '$s'"))

  override implicit def regex(r: Regex): MyParser[String] = loc =>
    r.findPrefixOf(loc.currentPos) match {
      case Some(s) => consume(loc, s)
      case _ => Failure(loc.toError(s"expected regex '$r'"))
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
      case Failure(e, false) => p2(loc).mapError(_.pushOther(e).flatten)
      case r => r
    }

  // Exercise 15: Implement the rest of the primitives, including run, using this representation of Parser, and try running your JSON parser on various inputs.
  override def flatMap[A, B](p: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] = loc =>
    p(loc) match {
      case Success(v, ahead) => f(v)(ahead)
      case Failure(e, c) => Failure(e, c)
    }

  override def recoverWith[A, B >: A](p: MyParser[A])(f: (Failure) => MyParser[B]): MyParser[B] = loc =>
    p(loc) match {
      case failure: Failure => f(failure)(loc)
      case success => success
    }

  override def describe[A](p: MyParser[A], description: String): MyParser[A] = loc =>
    p(loc).mapError(_.push(loc, description))

  override def important[A](p: MyParser[A]): MyParser[A] = loc =>
    p(loc).commit

  override def lick[A](p: MyParser[A]): Boolean = ???
}
