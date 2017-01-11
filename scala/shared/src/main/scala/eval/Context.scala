package eval


class Context(val bindings: Map[Val.Sym, Val], val module: Val.Sym) {

  def +(bind: (Val.Sym, Val)) = new Context(bindings + bind, module)

  def ++(bs: Iterable[(Val.Sym, Val)]): Context = Context(bindings ++ bs, module)

  def apply(name: Val.Sym): Option[Val] = bindings.get(name)

}

object Context {

  def apply(bindings: Map[Val.Sym, Val], module: Val.Sym): Context = new Context(bindings, module)

  def apply(module: Val.Sym)(bindings: (Val.Sym, Val)*): Context = new Context(bindings.toMap, module)

  def empty(module: Val.Sym): Context = new Context(Map.empty, module)

}
