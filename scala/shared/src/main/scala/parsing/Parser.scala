package parsing

import fastparse.WhitespaceApi

//noinspection ForwardReference
class Parser {

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    val comment = P(";" ~/ CharsWhile(_ != '\n'))
    NoTrace((CharsWhile(_.isWhitespace) | comment).rep)
  }

  import White._
  import fastparse.noApi._


  val program: P[Seq[SVal]] =
    P(Start ~ expression.rep ~ End)


  val expression: P[SVal] = P(string | symbol | number | sexp | vec)


  val string: P[SVal.Str] =
    P("\"" ~~ CharsWhile(_ != '"').! ~ "\"")
    .map(SVal.Str)


  val symbol: P[SVal.Sym] =
    P((CharPred(isSymbolStartChar) ~~ CharsWhile(isSymbolChar, 0)).!)
    .map(SVal.Sym)


  val number: P[SVal.Num] =
    P(decimalNumber).map(SVal.Num)

  val digit: P0 = CharPred(_.isDigit)

  val decimalNumber: P[Double] =
    P((digit.repX(min = 1) ~ ("." ~ digit.repX).?).!)
    .map(_.toDouble)


  def list(start: P0, end: P0): P[List[SVal]] =
    (start ~/ (!end ~ expression).rep ~ end)
    .map(_.toList)

  val sexp: P[SVal.Sexp] =
    P(list("(", ")"))
    .map(SVal.Sexp)
  val vec: P[SVal.Vec] =
    P(list("[", "]"))
    .map(SVal.Vec)


  def isSymbolStartChar(c: Char): Boolean =
    c.isLetter || "+=_-*&^%$#@!~|\\/?><.".contains(c)

  def isSymbolChar(c: Char): Boolean =
    c.isDigit || isSymbolStartChar(c)

}

object Parser extends Parser
