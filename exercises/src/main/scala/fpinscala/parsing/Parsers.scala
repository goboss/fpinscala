package fpinscala.parsing

import fpinscala.testing.Prop.forAll
import fpinscala.testing.{Gen, Prop}

import scala.annotation.tailrec
import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  // Run the parser p returning ParseResult.
  def run[A](p: Parser[A])(input: String): ParseResult[A]

  // Create a parser that always succeeds with the provided value.
  def succeed[A](a: A): Parser[A]

  def fail[A](error: ParseError): Parser[A]

  // Run the first parser and if it fails then try the other one.
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  // Run parser p and if it succeeds apply the function f to the result creating a new parser.
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // If parser p fails convert it to other parser
  def recoverWith[A, B >: A](p: Parser[A])(f: Failure => Parser[B]): Parser[B]

  // Run parser p, but instead of returning its result return the part (slice) of input that was consumed.
  def slice[A](p: Parser[A]): Parser[String]

  // Create parser that accepts the string s
  implicit def string(s: String): Parser[String]

  // Create parser that accepts the regex r
  implicit def regex(r: Regex): Parser[String]

  // Decorate parser with ops
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  // Convert a to string parser
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
      else acc
    }

    go(n, succeed(List.empty))
  }

  // Exercise 5: We could also deal with non-strictness with a separate combinator like we did in chapter 7.
  // Try this here and make the necessary changes to your existing combinators.
  // What do you think of that approach in this instance?
  def map2Strict[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p1)(a => map(p2)(b => f(a, b)))
  def lazyP[A](parser: => Parser[A]): Parser[A]
  def manyStrict[A](p: Parser[A]): Parser[List[A]] =
    map2Strict(p, lazyP(manyStrict(p)))(_ :: _) or succeed(List.empty)

  def digit: Parser[String] =
    "\\d".r

  def maybeSpace: Parser[String] =
    "\\s*".r

  def space:  Parser[String] =
    "\\s+".r

  def double: Parser[Double] =
    regex("""-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?""".r).map(_.toDouble)

  def literal[A](lit: String)(a: A): Parser[A] =
    slice(lit).map(_ => a)

  def trimLeft[L, A](left: Parser[L], p: Parser[A]): Parser[A] =
    map2(slice(many(left)), p)((_, a) => a)

  def trimRight[A, R](p: Parser[A], right: Parser[R]): Parser[A] =
    map2(p, slice(many(right)))((a, _) => a)

  def skipRight[A, R](p: Parser[A], right:  => Parser[R]): Parser[A] =
    map2(p, slice(right))((a, _) => a)

  def trim[L, A, R](left: Parser[L], p: Parser[A], right: Parser[R]): Parser[A] =
    for {
      _ <- slice(left)
      a <- p
      _ <- slice(right)
    } yield a

  def trimSpace[A](p: Parser[A]): Parser[A] =
    trim(maybeSpace, p, maybeSpace)

  def manySep[S, A](s: Parser[S], p: Parser[A]): Parser[List[A]] =
    map2(p, many(trimLeft(s, p)))(_ :: _)

  // Exercise 6: Using flatMap and any other combinators, write the context-sensitive parser we couldn’t express earlier.
  def numberOfAs: Parser[List[Char]] =
    digit.flatMap(s => listOfN(s.toInt, char('a')))

  def endOfInput: Parser[String] =
    describe(regex("\\z".r), "unexpected trailing characters")

  def whole[A](p: Parser[A]): Parser[A] =
    skipRight(p, endOfInput)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = or(p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def ~>[B](p2: => Parser[B]): Parser[B] = self.trimLeft(p, p2)
    def <~[B](p2: => Parser[B]): Parser[A] = self.trimRight(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

    def recoverWith[B >: A](f: Failure => Parser[B]): Parser[B] =
      self.recoverWith[A, B](p)(f)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def many: Parser[List[A]] = self.many(p)
    def * : Parser[List[A]] = many

    def many1: Parser[List[A]] = self.many1(p)
    def + : Parser[List[A]] = many1

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
    def productLaw1[A, B](a: A, b: B)(in: Gen[String]): Prop =
      equal(succeed(a) ** succeed(b), succeed((a, b)))(in)

    def productLaw2[A](a: A)(in: Gen[String]): Prop =
      equal(fail(ParseError()) ** succeed(a), fail(ParseError()))(in)

    def productLaw3[A](a: A)(in: Gen[String]): Prop =
      equal(succeed(a) ** fail(ParseError()), fail(ParseError()))(in)

    def stringLaw1(in: Gen[String]): Prop =
      equal(string(""), succeed(""))(in)

    def stringLaw2(in: Gen[String]): Prop =
      forAll(in)(s => run(string(s))(s).isSuccess)

    def stringLaw3(in: Gen[String]): Prop =
      forAll(in)(s => run(string(s))(s.tail).isFailure)

    def regexLaw1(in: Gen[String]): Prop =
      forAll(in)(s => run(regex("".r))(s).isSuccess)

    def regexLaw2(in: Gen[String]): Prop =
      forAll(in)(s => run(regex(".*".r))(s).isSuccess)

    def succeedLaw(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(()))(s).isSuccess)

    def failLaw(in: Gen[String]): Prop =
      forAll(in)(s => run(fail(ParseError()))(s).isFailure)

    def sliceLaw(in: Gen[String]): Prop =
      forAll(in) { s =>
        run(slice(string(s)))(s) match {
          case Success(v, _) =>
            v == s
          case _ =>
            false
        }
      }

    def orLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      forAll(in) { s =>
        val result = run(p1 or p2)(s)
        (run(p1)(s), run(p2)(s)) match {
          case (succ1 @ Success(_, _), _) =>
            result == succ1
          case (Failure(_, false), succ2 @ Success(_, _)) =>
            result == succ2
          case _ =>
            result.isFailure
        }
      }

    def flatMapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p.flatMap(succeed), p)(in)

    def recoverWithLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(fail(ParseError()).recoverWith(_ => p), p)(in)
  }
}
