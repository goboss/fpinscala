package fpinscala.parsing

import org.scalatest.{Matchers, FlatSpec}

class JSONSpec extends FlatSpec with Matchers {
  import JSON._

  def runJson(input: String): ParseResult[JSON] = MyParsers.run(JSON.jsonParser(MyParsers))(input)

  // Exercise 9

  "JSON parser" should "parse null" in {
    runJson("null") shouldBe Success(JNull, Location("null", 4))
  }

  it should "fail to parse null" in {
    runJson("nul") shouldBe a[Failure]
  }

  it should "parse a number" in {
    runJson("123") shouldBe Success(JNumber(123), Location("123", 3))
  }

  it should "fail to parse a number" in {
    runJson("a") shouldBe a[Failure]
  }

  it should "parse a string" in {
    runJson("\"str\"") shouldBe Success(JString("str"), Location("\"str\"", 5))
  }

  it should "fail to parse a string" in {
    runJson("\"str") shouldBe a[Failure]
  }

  it should "parse a boolean" in {
    runJson("true") shouldBe Success(JBool(true), Location("true", 4))
    runJson("false") shouldBe Success(JBool(false), Location("false", 5))
  }

  it should "parse an array" in {
    val json = "[1, \"abc\", true, null]"
    runJson(json) shouldBe Success(JArray(Vector(JNumber(1), JString("abc"), JBool(true), JNull)), Location(json, 22))
  }

  it should "fail to parse an array" in {
    runJson("[1, 2") shouldBe a[Failure]
    runJson("[1, 2,]") shouldBe a[Failure]
    runJson("[1 2]") shouldBe a[Failure]
  }

}
