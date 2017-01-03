package js

trait Ast

object Ast {

  sealed trait Expr extends Ast
  final case class Num(value: Double) extends Expr
  final case class Str(value: String) extends Expr
  final case object Undefined extends Expr
  final case class Var(binding: String) extends Expr


  final case class Apply(function: Expr, args: Seq[Expr]) extends Expr
  final case class Lambda(args: Seq[String], body: Ast) extends Expr
  final case class BinaryOp(op: String, lhs: Expr, rhs: Expr) extends Expr
  final case class ParBlock(asts: Seq[Ast], last: Expr) extends Expr
  final case class Obj(props: (String, Expr)*) extends Expr
  final case class Ternary(cond: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr
  final case class Select(obj: Expr, property: String) extends Expr


  final case class Block(statements: Seq[Ast]) extends Ast
  final case class Let(bindings: (String, Ast.Expr)*) extends Ast
  final case class Assign(binding: String, value: Expr) extends Ast
  final case class Return(value: Expr) extends Ast
  final case class If(cond: Expr, ifTrue: Ast, ifFalse: Ast) extends Ast
  final case class Throw(exception: Expr) extends Ast
  final case object Empty extends Ast


}
