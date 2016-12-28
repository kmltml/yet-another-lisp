package parsing

import fastparse.core.Parsed
import fastparse.core.Parsed.{Failure, Success}
import utest._

import SVal._

object ParserTest extends TestSuite {

  def assertSuccess[T](expected: T, result: Parsed[T, _, _]): Unit = result match {
    case Success(actual, _) =>
      actual ==> expected
    case _ =>
      throw AssertionError(s"Expected successful parse, found $result", Seq.empty)
  }

  val tests = this {
    "expression" - {

      "parse a string literal" - {
        assertMatch(Parser.expression.parse("\"Hello, World!\"")) {
          case Success(SVal.Str("Hello, World!"), _) =>
        }
      }

      "parse a symbol" - {
        "an alphanumeric symbol" - {
          assertMatch(Parser.expression.parse("foo")) {
            case Success(SVal.Sym("foo"), _) =>
          }
        }
        "operator symbols" - {
          for(op <- Seq("+", "-", "*", "/", "=", "++", ">>=")) {
            assertMatch(Parser.expression.parse(op)) {
              case Success(SVal.Sym(`op`), _) =>
            }
          }
        }
        "a hyphen-separated symbol" - {
          assertMatch(Parser.expression.parse("hello-world")) {
            case Success(SVal.Sym("hello-world"), _) =>
          }
        }
        "greek letters" - {
          assertMatch(Parser.expression.parse("λ")) {
            case Success(SVal.Sym("λ"), _) =>
          }
        }
      }

      "parse a number" - {
        "integer" - {
          assertMatch(Parser.expression.parse("10")) {
            case Success(SVal.Num(10), _) =>
          }
        }
        "float" - {
          assertMatch(Parser.expression.parse("20.5")) {
            case Success(SVal.Num(20.5), _) =>
          }
        }
      }

      "parse an s-expression" - {
        "empty" - {
          assertMatch(Parser.expression.parse("()")) {
            case Success(SVal.Sexp(Nil), _) =>
          }
        }
        "simple function call" - {
          assertSuccess(SVal.Sexp(List(
            SVal.Sym("+"),
            SVal.Num(2),
            SVal.Num(20)
          )), Parser.expression.parse("(+ 2 20)"))
        }
      }
    }

    "program" - {
      "parse a simple program with couple defs" - {
        val res = Parser.program.parse(
          """
            |
            |(def (foo x) (+ x 3))
            |; an example comment
            |(def (main) (println (show (foo 1))))
          """.stripMargin)
        assertSuccess(Seq(
          Sexp(List(
            Sym("def"),
            Sexp(List(Sym("foo"), Sym("x"))),
            Sexp(List(Sym("+"), Sym("x"), Num(3)))
          )),
          Sexp(List(
            Sym("def"),
            Sexp(List(Sym("main"))),
            Sexp(List(
              Sym("println"),
              Sexp(List(
                Sym("show"),
                Sexp(List(Sym("foo"), Num(1)))
              ))
            ))
          ))
        ), res)
      }
    }
  }

}
