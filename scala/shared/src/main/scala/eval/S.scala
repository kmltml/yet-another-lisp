package eval

import eval.Val.Sym

object S {


  val Symbol = Sym("symbol")
  val Sexp   = Sym("sexp")
  val +      = Sym("+")
  val -      = Sym("-")
  val *      = Sym("*")
  val /      = Sym("/")
  val λ      = Sym("λ")
  val Lambda = Sym("lambda")

}
