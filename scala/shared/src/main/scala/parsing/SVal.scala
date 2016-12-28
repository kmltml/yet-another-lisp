package parsing

sealed trait SVal

object SVal {

  case class Sym(name: String) extends SVal
  case class Sexp(elems: List[SVal]) extends SVal
  case class Str(value: String) extends SVal
  case class Num(value: Double) extends SVal
  case class Quot(symbol: String) extends SVal
  case class Vec(elems: List[SVal]) extends SVal

}
