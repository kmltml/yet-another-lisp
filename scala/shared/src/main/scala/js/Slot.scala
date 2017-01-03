package js

object Slot {


  val id: Slot = identity
  val ret: Slot = Ast.Return(_)
  val ignore: Slot = _ => Ast.Empty

  def store(binding: String): Slot = Ast.Assign(binding, _)

}
