import fpinscala.parsing._
import MyParsers._
import JSON._

run(jsonParser(MyParsers))("nul")

run((char('a') | char('b')) | char('c'))("d")
