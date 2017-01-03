package eval

import eval.Val.Sym

import scala.language.dynamics

object S extends Dynamic {


//  val Symbol = Sym("symbol")
//  val Sexp   = Sym("sexp")
  val +       = Sym("+")
  val `match` = Sym("match")
  val data    = Sym("data")
//  val -      = Sym("-")
//  val *      = Sym("*")
//  val /      = Sym("/")
//  val λ      = Sym("λ")
//  val Lambda = Sym("lambda")
//  val Def    = Sym("def")

//  def selectDynamic(sym: String): Sym = Sym(sym.toLowerCase)

  object selectDynamic {

    def apply(name: String): Sym = Sym(name)

    def unapply(sym: Sym): Option[String] = Some(sym.name)

  }

}
