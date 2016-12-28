package js

import eval.{S, Val}

object Compiler {

  private var nextId = 0

  def freshIdentifier(): String = {
    val ret = "$_" + nextId
    nextId += 1
    ret
  }

  def compileExpr(v: Val): Frag = v match {
    case Val.Num(n) => Simple(Ast.Num(n))
    case Val.Sym(s) => Simple(Ast.Var(s))
    case Val.Sexp(fn :: args) => call(fn, args)
  }

  private def call(fn: Val, args: List[Val]): Frag = fn match {
    case S.+ => binaryOp("+", args)
    case S.- => binaryOp("-", args)
    case S./ => binaryOp("/", args)
    case S.* => binaryOp("*", args)
    case S.λ | S.Lambda =>
      val List(Val.Sexp(params), body) = args
      val paramNames = params.map {
        case Val.Sym(s) => s
      }
      compileExpr(body) match {
        case Simple(e) => Simple(Ast.Lambda(paramNames, e))
        case Complex(f) =>
          val statements = f(Slot.ret)
          Simple(Ast.Lambda(paramNames, Ast.Block(statements)))
      }

    case _ =>
      val fnUnified = compileExpr(fn).unify
      val argsUnified = args.map(compileExpr(_).unify)
      val stats = fnUnified._1 ++ argsUnified.flatMap(_._1)
      stats match {
        case Seq() => Simple(Ast.Apply(fnUnified._2, argsUnified.map(_._2)))
        case _ => Complex { slot =>
          stats :+ slot(Ast.Apply(fnUnified._2, argsUnified.map(_._2)))
        }
      }

  }

  private def binaryOp(op: String, args: List[Val]): Frag = {
    val unified = args.map(compileExpr).map(_.unify)
    val before = unified.flatMap(_._1)
    val expr = unified.map(_._2).reduceLeft((a, b) => Ast.BinaryOp(op, a, b))
    before match {
      case Nil => Simple(expr)
      case _ => Complex { slot =>
        before :+ slot(expr)
      }
    }
  }

}
