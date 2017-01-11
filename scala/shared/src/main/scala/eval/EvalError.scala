package eval

sealed trait EvalError

object EvalError {

  final case class UnboundSymbol(symbol: Val.Sym) extends EvalError

}
