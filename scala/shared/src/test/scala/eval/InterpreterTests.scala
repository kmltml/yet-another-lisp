package eval

import utest._

object InterpreterTests extends TestSuite{

  def sexp(vals: Val*): Val.Sexp = Val.Sexp(vals.toList)
  def n(n: Double): Val.Num = Val.Num(n)
  def s(s: String): Val.Str = Val.Str(s)

  def eval(v: Val): Val = Interpreter.eval(v, Interpreter.defaultContext)(Interpreter.defaultGlobal)

  implicit def sym(s: Symbol): Val.Sym = Val.Sym(s.name)

  val tests = apply {
    "2 + 3 = 5" - {
      eval(sexp('+, n(2), n(3))) ==> n(5)
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
  }

}
