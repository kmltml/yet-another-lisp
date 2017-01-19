package eval

import parsing.SVal

import scala.collection.mutable

sealed trait Val

object Val {

  final class Sym private[Val](val name: String) extends Val {

    override def equals(obj: Any): Boolean = this eq obj.asInstanceOf[AnyRef]

    override def toString: String = s"'$name"

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

  sealed trait Fun extends Val
  case class Builtin(run: List[Val] => Val) extends Fun
  case class Lambda(args: List[Val.Sym], body: Val, ctxt: Context) extends Fun
  case class Def(bodies: List[(List[Val], Val)], ctxt: Context) extends Fun {
    def numParams = bodies.head._1.size
  }

  case class Data(constructor: Val.Sym, members: Seq[Val]) extends Val

  case class Native(value: Any) extends Val // Used to store objects in js environment, do not overuse

  def desugar(sval: SVal): Val = sval match {
    case SVal.Sexp(vals) => Sexp(vals map desugar)
    case SVal.Sym(s)     => Sym(s)
    case SVal.Num(n)     => Num(n)
    case SVal.Str(s)     => Str(s)
    case SVal.Quot(s)    => Sexp(List(S.symbol, Sym(s)))
    case SVal.Vec(vals)  => Sexp(S.sexp :: (vals map desugar))
  }

}
