package eval

import parsing.SVal

import scala.collection.mutable

sealed trait Val

object Val {

  final class Sym private[Val](val name: String) extends Val {

    override def equals(obj: Any): Boolean = this eq obj.asInstanceOf[AnyRef]

  }
  object Sym {

    private val internedSymbols: mutable.Map[String, Sym] = mutable.Map.empty

    def apply(name: String): Sym =
      internedSymbols.getOrElseUpdate(name, new Sym(name))

    def unapply(sym: Sym): Option[String] = Some(sym.name)

  }

  case class Num(value: Double) extends Val
  case class Str(value: String) extends Val
  case class Sexp(value: List[Val]) extends Val



  def desugar(sval: SVal): Val = sval match {
    case SVal.Sexp(vals) => Sexp(vals map desugar)
    case SVal.Sym(s)     => Sym(s)
    case SVal.Num(n)     => Num(n)
    case SVal.Str(s)     => Str(s)
    case SVal.Quot(s)    => Sexp(List(S.Symbol, Sym(s)))
    case SVal.Vec(vals)  => Sexp(S.Sexp :: (vals map desugar))
  }

}
