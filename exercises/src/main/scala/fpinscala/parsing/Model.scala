package fpinscala.parsing

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

  def hasMore: Boolean = offset < input.length
}

// Exercise 18: Change `the representation of ParseError to keep track of errors that occurred in other branches of the parser.
case class ParseError(
  stack: List[(Location, String)] = List.empty,
  otherFailures: List[ParseError] = List.empty
) {

  // Exercise 16: Come up with a nice way of formatting a ParseError for human consumption.
  def show: String = {
    stack
      .groupBy { case (loc, _) => loc }
      .map { case (loc, errors) =>
        val errorDesc = errors.map(_._2).mkString(" -> ")
        val otherDesc = otherFailures.map(_.showLocation(loc)).map("\t" + _).mkString("\n")

        s"Errors at ${loc.line}:${loc.col}, near '${loc.currentLine}':\n" +
          s"\t$errorDesc\n" + (if (otherDesc.nonEmpty) "Also:\n" + otherDesc else "")

      }
      .mkString("\n")
  }

  def showLocation(location: Location): String =
    stack.filter(_._1 == location).map(_._2).mkString(" -> ")

  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack, otherFailures = otherFailures.map(_.push(loc, msg)))

  def pushOther(error: ParseError): ParseError =
    copy(otherFailures = error :: otherFailures)

  def flatten: ParseError = {
    def go(remain: List[ParseError], acc: List[ParseError]): List[ParseError] =
      remain match {
        case Nil =>
          acc.reverse
        case h :: t =>
          go(t ++ go(h.otherFailures, List.empty), h.copy(otherFailures = List.empty) :: acc)
      }

    copy(otherFailures = go(otherFailures, List.empty))
  }
}

trait ParseResult[+A] {
  def isSuccess: Boolean
  def isFailure: Boolean = !isSuccess

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
  override def isSuccess: Boolean = true

  def advanceSuccess(n: Int): ParseResult[A] = this match {
    case Success(a, m) => Success(a, m.advanceBy(n))
    case _ => this
  }
}
case class Failure(error: ParseError, isCommited: Boolean = false) extends ParseResult[Nothing] {
  override def isSuccess: Boolean = false

  def addCommit(isCommitted: Boolean): ParseResult[Nothing] = this match {
    case Failure(e,c) => Failure(e, c || isCommitted)
    case _ => this
  }

  override def toString: String = error.show
}
