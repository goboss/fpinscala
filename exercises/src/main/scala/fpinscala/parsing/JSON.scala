package fpinscala.parsing

import scala.language.higherKinds

// Exercise 9: Create JSON parser from scratch, without worrying about the representation of Parser
trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val comma = trimSpace(",")
    val str = """(([^"\\]+|(\\[\\\/"bfnrtu])+|(\\u[0-9a-fA-F]{4}))+)""".r

    def jnull: Parser[JNull.type] =
      literal("null")(JNull)
    def jnumber: Parser[JNumber] =
      double.map(JNumber)
    def jstring: Parser[JString] =
      trim("\"", str, "\"").map(JString)
    def jbool: Parser[JBool] =
      literal("true")(JBool(true)) | literal("false")(JBool(false))
    def jarray: Parser[JArray] =
      trim(trimSpace("["), manySep(comma, jvalue), trimSpace("]")).map(l => JArray(l.toIndexedSeq))
    def jmapping: Parser[(String, JSON)] =
      jstring.map(_.get) ** (trimSpace(":") ~> jvalue)
    def jobject: Parser[JObject] =
      trim(
        "{",
        trimSpace(manySep(comma, jmapping)),
        "}"
      ).map(ms => JObject(ms.toMap))
    def jvalue: Parser[JSON] = jnull | jnumber | jstring | jbool | jarray | jobject

    // in case of json it would be better to strip input of all spaces, since it is not space sensitive, but this is fun :)
    trimSpace(jvalue)
  }

  def test = {
    val jsonTxt = """
      {
        "Company name" : "Microsoft Corporation",
        "Ticker"  : "MSFT",
        "Active"  : true,
        "Price"   : 30.66,
        "Shares outstanding" : 8.38e9,
        "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
      }
    """

    MyParsers.run(jsonParser(MyParsers))(jsonTxt)
  }
}
