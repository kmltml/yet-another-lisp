package js

object Slot {


  val id: Slot = identity
  val ret = Ast.Return(_)

  def store(binding: String): Slot = Ast.Assign(binding, _)

}
