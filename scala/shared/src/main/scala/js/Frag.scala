package js

import js.Ast.Expr

sealed trait Frag {

  def unify: (Seq[Ast], Ast.Expr) = this match {
    case Simple(e) => (Seq.empty, e)
    case Complex(sts) =>
      val res = Compiler.freshIdentifier()
      val slot = Slot.store(res)
      val statements = sts(slot)
      (Ast.Let(res -> Ast.Undefined) +: statements, Ast.Var(res))
  }

}

case class Simple(expression: Ast.Expr) extends Frag

final case class Complex(statements: Slot => Seq[Ast]) extends Frag


