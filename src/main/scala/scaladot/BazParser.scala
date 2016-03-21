package scaladot

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.CharArrayReader.EofCh

/**
  * Created on 3/18/16.
  */

class BazLexer  extends StdLexical with ImplicitConversions {

  override def token: Parser[Token] =
    ( string ^^ StringLit
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
      | '-' ~ whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l) }
      | '-' ~ whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num) }
      | number ^^ NumericLit
      | EofCh ^^^ EOF
      | delim
      | '\"' ~> failure("Unterminated string")
      | id ^^ checkKeyword
      | failure("Illegal character")
      )

  // def idcont = letter | digit | underscore
  def id = rep(letter | digit | elem("underscore", _=='_')) ^^ { _ mkString "" }

  // def underscore: Parser[String] = elem('_')

  def checkKeyword(strRep: String) = {
    if (reserved contains strRep) Keyword(strRep) else Identifier(strRep)
  }

  /** A string is a collection of zero or more Unicode characters, wrapped in
    *  double quotes, using backslash escapes (cf. http://www.json.org/).
    */
  def string = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }

  override def whitespace = rep(whitespaceChar)

  def number = intPart ^^ { case i => i }
  def intPart = zero | intList
  def intList = nonzero ~ rep(digit) ^^ {case x ~ y => (x :: y) mkString ""}

  private def optString[A](pre: String, a: Option[A]) : String= a match {
    case Some(x) => pre + x.toString
    case None => ""
  }

  def zero: Parser[String] = '0' ^^^ "0"
  def nonzero = elem("nonzero digit", d => d.isDigit && d != '0')
  def exponent = elem("exponent character", d => d == 'e' || d == 'E')
  def sign = elem("sign character", d => d == '-' || d == '+')

  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
      |'\\' ~ '\\' ^^^ "\\"
      |'\\' ~ '/'  ^^^ "/"
      |'\\' ~ 'b'  ^^^ "\b"
      |'\\' ~ 'f'  ^^^ "\f"
      |'\\' ~ 'n'  ^^^ "\n"
      |'\\' ~ 'r'  ^^^ "\r"
      |'\\' ~ 't'  ^^^ "\t"
      |'\\' ~ 'u' ~> unicodeBlock)

  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  def hexDigit = elem("hex digit", hexDigits.contains(_))

  private def unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
    case a ~ b ~ c ~ d =>
      new String(Array(Integer.parseInt(List(a, b, c, d) mkString "", 16)), 0, 1)

  }
}

class BazParser  extends StdTokenParsers with ImplicitConversions {

  type Tokens = BazLexer
  val lexical = new Tokens

  // Configure lexical parsing
  lexical.reserved ++= List("baz", "foo", "bar")
  lexical.delimiters ++= List("{", "}", "[", "]", "(", ")", ";", ",", "->", "--","\"")

  import lexical._


  def baz = "baz" ~> ID ~ ("=>" ~> foolist) ^^
    { case id ~ foos => Baz(id, foos:_*) }

  def foolist =  "[" ~> (repsep(foo,",")) <~ "]"

  def foo = "foo" ~> ID ~ ("->" ~> barlist) ^^
    { case id ~ bars => Foo(id, bars:_*) }

  def barlist =  "{" ~> (repsep(bar,",")) <~ "}"

  def bar = "bar" ~> ID ^^
    { case id => Bar(id) }

  def ID:Parser[String] = "(" ~> (IDs | IDi) <~ ")"
  def IDs = accept("string", { case StringLit(n) => n })
  def IDi = accept("identifier", { case Identifier(n) => n})


  case class P(z: Int*)

  def  pq = ("[" ~> ps <~ "]") ^^ {P(_:_*)}
  def  ps = repsep(px, ",")
  def  px = "0" ^^^ 0

  val s: Parser[String] = "z"




  case class Q(z: Int*)

  def  qq: Parser[Q] = (qk1 ~> qs <~ qk2) ^^ {Q(_:_*)}
  def  qs: Parser[Seq[Int]] = repsep(qx, qv)
  def  qx: Parser[Int] = "0" ^^^ 0
  def  qv: Parser[String] = "," ^^^ ","
  def  qk1: Parser[String] = "[" ^^^ "["
  def  qk2: Parser[String] = "]" ^^^ "]"



}

object Baz extends BazParser {

  def parse(input: String) =
    phrase(baz)(new lexical.Scanner(input)) match {
      case Success(result, _) => println("Success!"); Some(result)
      case n @ _ => println(n); None
    }


  def main(args: Array[String]) {
    val x = parse("""
      baz("O") => [
        foo("A") -> { bar("x1"), bar("x2"), bar("x3") },
        foo("B") -> { bar("y1"), bar("y2") },
        foo("C") -> { bar("z1") },
      ]
                  """)

    println(x)

  }
}

abstract class BazComponent {
  override def toString = {
    val b = new StringBuilder
    buildString(0, b)
    b.toString()
  }

  private def indent(level: Int, b: StringBuilder) {
    for (i <- 0 to level) b append ' '
  }

  def buildString(implicit level: Int, b: StringBuilder) {

    def between(sep: String, things: Seq[BazComponent])(implicit lev: Int) {
      var first = true
      for (t <- things) {
        if (first) first = false else b append sep
        t.buildString(lev, b)
      }
    }

    def betweenList(before: String, sep: String, after: String, things: Seq[BazComponent])(implicit lev: Int) {
      if (!things.isEmpty) {
        b append before
        between(sep, things)(lev)
        b append after
      }
    }

    this match {
      case Bar(id, foos @ _*) =>
        indent(level,b)
        b append "bar " append id
        between(" -> ", foos)
      case Foo(id, bars @ _*) =>
        indent(level,b)
        b append "Foo " append id
        between(" -> ", bars)
      case Baz(id, foos @ _*) =>
        indent(level,b)
        b append "BAZ " append id
        between(" => ", foos)
    }
  }

}

trait BarComponent extends BazComponent
trait FooComponent extends BazComponent
trait RootComponent extends BazComponent

case class Bar(id: String, foos: Foo*) extends BazComponent with BarComponent
case class Foo(id: String, bars: Bar*) extends FooComponent with BarComponent
case class Baz(id: String, foos: Foo*) extends BazComponent with RootComponent
