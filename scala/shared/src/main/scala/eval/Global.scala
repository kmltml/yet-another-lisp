package eval

class Global(val moduleContexts: Map[Val.Sym, ModuleContext])

class ModuleContext(val defs: Map[Val.Sym, Val], val imports: Seq[Val.Sym]) {

  def ++(ds: Iterable[(Val.Sym, Val)]): ModuleContext =
    new ModuleContext(defs ++ ds, imports)

}
