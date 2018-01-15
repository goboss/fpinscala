package fpinscala.parsing

import org.scalatest.{Matchers, FlatSpec}

class JSONSpec extends FlatSpec with Matchers {
  import JSON._

  def runJson(input: String): ParseResult[JSON] = MyParsers.run(JSON.jsonParser(MyParsers))(input)

  // Exercise 9

  "JSON parser" should "parse null" in {
    runJson("null") shouldBe Success(JNull, Location("null", 4))
  }

  it should "parse a number" in {
    runJson("-123.45") shouldBe Success(JNumber(-123.45), Location("-123.45", 7))
  }

  it should "fail to parse a char as number" in {
    runJson("a") shouldBe a[Failure]
  }

  it should "parse a string" in {
    runJson("\"str\"") shouldBe Success(JString("str"), Location("\"str\"", 5))
  }

  it should "fail to parse a string with no closing quote" in {
    runJson("\"str") shouldBe a[Failure]
  }

  it should "fail to parse a string with no opening quote" in {
    runJson("str\"") shouldBe a[Failure]
  }

  it should "fail to parse a string with no quotes" in {
    runJson("str") shouldBe a[Failure]
  }

  it should "parse a boolean" in {
    runJson("true") shouldBe Success(JBool(true), Location("true", 4))
    runJson("false") shouldBe Success(JBool(false), Location("false", 5))
  }

  it should "parse an array" in {
    val json = "[1, \"abc\", true, null]"
    runJson(json) shouldBe Success(JArray(Vector(JNumber(1), JString("abc"), JBool(true), JNull)), Location(json, 22))
  }

  it should "fail to parse an array with no closing brace" in {
    runJson("[1, 2") shouldBe a[Failure]
  }

  it should "fail to parse an array with no opening brace" in {
    runJson("1, 2]") shouldBe a[Failure]
  }

  it should "fail to parse an array with no braces" in {
    runJson("1, 2") shouldBe a[Failure]
  }

  it should "fail to parse an array with a dangling comma" in {
    runJson("[1, 2,]") shouldBe a[Failure]
  }

  it should "fail to parse an array with no comma between elements" in {
    runJson("[1 2]") shouldBe a[Failure]
  }

  it should "parse an object" in {
    val json =
      """
         {
           "num": 1.23,
           "str": "test",
           "arr": [1, true],
           "obj": {
             "x": null,
             "other": {
               "name": "it yourself"
             }
           }
         }
      """

    runJson(json) shouldBe Success(
      JObject(Map(
        "num" -> JNumber(1.23),
        "str" -> JString("test"),
        "arr" -> JArray(Vector(JNumber(1), JBool(true))),
        "obj" -> JObject(Map(
          "x" -> JNull,
          "other" -> JObject(Map("name" -> JString("it yourself")))
        ))
      )),
      Location(json, 241)
    )
  }

  it should "fail to parse an object with dangling comma" in {
    val json = """{"x": 1,}"""
    runJson(json) shouldBe a[Failure]
  }

  it should "fail to parse an object with no closing brace" in {
    val json = """{"x": 1"""
    runJson(json) shouldBe a[Failure]
  }

  it should "fail to parse an object with no opening brace" in {
    val json = """"x": 1}"""
    runJson(json) shouldBe a[Failure]
  }

  it should "fail to parse an object with no braces" in {
    val json = """"x": 1"""
    runJson(json) shouldBe a[Failure]
  }

  it should "fail to parse an object with unpaired closing brace" in {
    val json = """{"o": {"x": 1}"""
    runJson(json) shouldBe a[Failure]
  }

  it should "fail to parse an object with no quotes around key name" in {
    val json = """{x: 1}"""
    runJson(json) shouldBe a[Failure]
  }

}
