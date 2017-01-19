package eval

import utest._

object InterpreterTests extends TestSuite{

  def sexp(vals: Val*): Val.Sexp = Val.Sexp(vals.toList)
  def v(vals: Val*): Val.Sexp = Val.Sexp(sym('sexp) +: vals.toList)
  def n(n: Double): Val.Num = Val.Num(n)
  def s(s: String): Val.Str = Val.Str(s)

  def eval(v: Val): Val = Interpreter.eval(v, Interpreter.defaultContext)(Interpreter.defaultGlobal).value

  implicit def sym(s: Symbol): Val.Sym = Val.Sym(s.name)
  def sym(s: String): Val.Sym = Val.Sym(s)

  val tests = apply {
    "2 + 3 = 5" - {
      eval(sexp('+, n(2), n(3))) ==> n(5)
    }
    "comparison operators" - {
      eval(sexp('= , n(2), n(2))) ==> sym('true)
      eval(sexp('= , n(1), n(2))) ==> sym('false)
      eval(sexp('> , n(3), n(2))) ==> sym('true)
      eval(sexp('> , n(2), n(2))) ==> sym('false)
      eval(sexp('> , n(1), n(2))) ==> sym('false)
      eval(sexp('>=, n(3), n(2))) ==> sym('true)
      eval(sexp('>=, n(2), n(2))) ==> sym('true)
      eval(sexp('>=, n(1), n(2))) ==> sym('false)
      eval(sexp('< , n(3), n(2))) ==> sym('false)
      eval(sexp('< , n(2), n(2))) ==> sym('false)
      eval(sexp('< , n(1), n(2))) ==> sym('true)
      eval(sexp('<=, n(3), n(2))) ==> sym('false)
      eval(sexp('<=, n(2), n(2))) ==> sym('true)
      eval(sexp('<=, n(1), n(2))) ==> sym('true)

    }
    "an if expression" - {
      eval(sexp('if, sexp('=, n(1), n(1)), n(1), n(0))) ==> n(1)
      eval(sexp('if, sexp('=, n(1), n(2)), n(1), n(0))) ==> n(0)
    }
    "lambda abstraction and application" - {
      eval(sexp(sexp('λ, sexp('x), sexp('+, 'x, n(1))), n(2))) ==> n(3)
    }
    "function definition and usage" - {
      Interpreter.evalProgram(List(
        sexp('def, sexp('foo, 'x), sexp('+, n(1), 'x)),
        sexp('foo, n(4))
      ))._2 ==> n(5)
    }
    "recursive function definition and usage" - {
      Interpreter.evalProgram(List(
        sexp('def, sexp('add, 'x, 'y), sexp('if, sexp('=, n(0), 'x), 'y, sexp('add, sexp('-, 'x, n(1)), sexp('+, 'y, n(1))))),
        sexp('add, n(5), n(10))
      ))._2 ==> n(15)
      "simple adt definition and usage" - {
        Interpreter.evalProgram(List(
          sexp('data, sexp('Option, 'x),
            sexp('Some, 'x),
            sexp('None)),
          sexp('Some, 'None)
        ))._2 ==> Val.Data(Val.Sym("Some"), Seq(Val.Data(Val.Sym("None"), Seq.empty)))
      }
    }
    "simple pattern match on numbers" - {
      eval(sexp('match, n(2),
        sexp(n(1), s("one")),
        sexp(n(2), s("two")),
        sexp(n(3), s("three")),
        sexp('_, s("lots")))) ==> s("two")
    }
    "pattern match on adt" - {
      Interpreter.evalProgram(List(
        sexp('data, sexp('Option, 'x),
          sexp('Some, 'x),
          sexp('None)),
        sexp('match, sexp('Some, n(2)),
          sexp('None, n(0)),
          sexp(sexp('Some, n(1)), n(10)),
          sexp(sexp('Some, 'v), 'v))
      ))._2 ==> n(2)
    }
    "pattern matching in def" - {
      Interpreter.evalProgram(List(
        sexp('def,
          sexp('factorial, n(1)), n(1),
          sexp('factorial, 'n), sexp('*, 'n, sexp('factorial, sexp('-, 'n, n(1))))
        ),
        sexp('factorial, n(6))
      ))._2 ==> n(720)
    }
    "list map" - {
      Interpreter.evalProgram(List(
        sexp('data, sexp('List, 'a),
          sexp('::, 'a, sexp('List, 'a)),
          sexp('Nil)),
        sexp('def,
          sexp('map, '_, 'Nil), 'Nil,
          sexp('map, 'f, sexp('::, 'h, 't)),
            sexp('::, sexp('f, 'h), sexp('map, 'f, 't))),
        sexp('map, sexp('λ, sexp('x), sexp('+, 'x, n(1))), sexp('::, n(1), sexp('::, n(2), 'Nil)))
      ))._2 ==> Val.Data('::, Seq(n(2), Val.Data('::, Seq(n(3), Val.Data('Nil, Seq())))))
    }
    "let form" - {
      eval(
        sexp('let,
          sexp(
            sexp('x, n(1)),
            sexp('y, sexp('+, 'x, n(2)))),
          sexp('+, 'x, 'y))
      ) ==> n(4)
    }
    "deep recursion" - {
      Interpreter.evalProgram(List(
        sexp('def,
          sexp('foo, n(0)), n(0),
          sexp('foo, 'n), sexp('foo, sexp('-, 'n, n(1)))),
        sexp('foo, n(100000))
      ))
    }
    "lexical scope in defs" - {
      Interpreter.evalProgram(List(
        sexp('def, sexp('x), n(10)),
        sexp('def, sexp('foo), sexp('x)),
        sexp('let, sexp(
          sexp('x, sexp('λ, sexp(), n(20)))
        ), sexp('foo))
      ))._2 ==> n(10)
    }
    "simple non-function def" - {
      Interpreter.evalProgram(List(
        sexp('def, 'foo, n(10)),
        sexp('+, 'foo, n(3))
      ))._2 ==> n(13)
    }
    "partial application of built-in functions" - {
      eval(sexp(sexp('+, n(2)), n(3))) ==> n(5)
      eval(sexp(sexp('-, n(2)), n(3))) ==> n(-1)
      eval(sexp(sexp('*, n(2)), n(3))) ==> n(6)
      eval(sexp(sexp('/, n(2)), n(3))) ==> n(2.0/3.0)
    }
    "partial application of lambdas" - {
      eval(sexp(sexp(sexp('λ, sexp('x, 'y), sexp('+, 'x, 'y)), n(2)), n(3))) ==> n(5)
    }
    "partial application of defs" - {
      Interpreter.evalProgram(List(
        sexp('def, sexp('add, 'x, 'y), sexp('+, 'x, 'y)),
        sexp(sexp('add, n(1)), n(2))
      ))._2 ==> n(3)
    }
    "sequential dependence of constant defs" - {
      Interpreter.evalProgram(List(
        sexp('def, 'x, n(1)),
        sexp('def, 'y, sexp('+, 'x, n(10))),
        'y
      ))._2 ==> n(11)
    }
    "sequential dependence of constant and function defs" - {
      Interpreter.evalProgram(List(
        sexp('def, sexp('++, 'x), sexp('+, 'x, n(1))),
        sexp('def, 'x, sexp('++, n(10))),
        'x
      ))._2 ==> n(11)
    }
    "point-free list sum" - {
      Interpreter.evalProgram(List(
        sexp('data, sexp('List, 'a),
          sexp('::, 'a, sexp('List, 'a)),
          sexp('Nil)),
        sexp('def,
          sexp('foldl, '_, 'i, 'Nil), 'i,
          sexp('foldl, 'f, 'i, sexp('::, 'a, 'as)),
            sexp('foldl, 'f, sexp('f, 'i, 'a), 'as)),
        sexp('def, 'sum, sexp('foldl, '+, n(0))),
        sexp('sum, sexp('::, n(1), sexp('::, n(2), sexp('::, n(3), sexp('::, n(4), 'Nil)))))))._2 ==> n(10)
    }
    "pattern matching on sexps" - {
      eval(sexp('match, sexp('sexp, n(1), n(2), n(3)),
        sexp(sexp('sexp, 'x, 'y), sexp('*, 'x, 'y)),
        sexp(sexp('sexp, n(1), n(2), 'n), 'n))) ==> n(3)

      eval(sexp('match, sexp('sexp, n(1), n(2)),
        sexp(sexp('sexp, 'x, 'y), sexp('*, 'x, 'y)),
        sexp(sexp('sexp, n(1), n(2), 'n), 'n))) ==> n(2)

      eval(sexp('match, sexp('sexp),
        sexp(sexp('sexp), n(1)))) ==> n(1)
    }
    "pattern matching rest pattern on sexps" - {
      eval(sexp('match, sexp('sexp, n(1), n(2), n(3)),
        sexp(sexp('sexp, 'x, sexp(sym("..."), '_)), 'x))) ==> n(1)
    }
    "splicing sexps" - {
      eval(v(n(1), sexp(sym("..."), v(n(2), n(3))), n(4), sexp(sym("..."), v(n(5))))) ==>
        sexp(n(1), n(2), n(3), n(4), n(5))
    }
    "source quoting" - {
      val foo = sexp('foo, n(10), sexp('+, n(2), s("four")))
      eval(sexp('quote, foo)) ==> foo
      eval(sexp('quote, 'foo)) ==> sym('foo)
    }
  }

}
