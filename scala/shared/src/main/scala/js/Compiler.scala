package js

import eval.{S, Val}
import js.Ast.Block

object Compiler {

  private var nextId = 0

  def freshIdentifier(): String = {
    val ret = "$_" + nextId
    nextId += 1
    ret
  }

  def compileStatement(v: Val): Seq[Ast] = v match {
    case Val.Sexp(S.`def` :: rest) => Seq(compileDef(rest))

    case Val.Sexp(S.data :: Val.Sexp(_) :: constrs) =>
      // TODO for now ignoring type name and type parameters
      constrs.map {
        case Val.Sexp(Val.Sym(name) :: Nil) =>
          Ast.Let(name -> Ast.Obj("tag" -> Ast.Str(name)))

        case Val.Sexp(Val.Sym(name) :: params) =>
          val fieldNames = params.indices.map(i => s"_$i")
          val fields = ("tag" -> Ast.Str(name)) +: fieldNames.map(n => n -> Ast.Var(n))
          Ast.Let(name -> Ast.Lambda(fieldNames, Ast.Obj(fields: _*)))
      }

    case _ => compileExpr(v) match {
      case Simple(ast) => Seq(ast)
      case Complex(f)  => f(Slot.id)
    }
  }

  def compileExpr(v: Val): Frag = v match {
    case Val.Num(n) => Simple(Ast.Num(n))
    case Val.Str(s) => Simple(Ast.Str(s))
    case Val.Sym(s) => Simple(Ast.Var(s))
    case Val.Sexp(fn :: args) => call(fn, args)
  }

  private def compileDef(vs: List[Val]): Ast = {
    val List(Val.Sexp(Val.Sym(name) :: args), body) = vs
    val argNames = args.map {
      case Val.Sym(n) => n
    }
    val Simple(lambda) = call(S.lambda, List(Val.Sexp(args), body))
    Ast.Let(name -> lambda)
  }

  private def call(fn: Val, args: List[Val]): Frag = fn match {
    case S.+ => binaryOp("+", args)
    case S.- => binaryOp("-", args)
    case S./ => binaryOp("/", args)
    case S.* => binaryOp("*", args)
    case S.`=` => binaryOp("===", args)

    case S.λ | S.lambda =>
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

    case S.`if` =>
      val List(cond, iftrue, iffalse) = args
      Complex { slot =>
        val condUnified = compileExpr(cond).unify
        val t = toBlock(compileExpr(iftrue), slot)
        val f = toBlock(compileExpr(iffalse), slot)
        condUnified._1 :+ Ast.If(condUnified._2, t, f)
      }


    case S.`match` =>

      case class Pat(lets: Seq[String], condition: Ast.Expr => Ast.Expr, value: Frag)

      val matchee :: patterns = args
      val pats = patterns.map {
        case Val.Sexp(List(pat, value)) =>
          val (lets, expr) = pattern(pat)
          Pat(lets, expr, compileExpr(value))
      }
      val lets = pats.flatMap(_.lets)
      val (matcheeBefore, matcheeVal) = compileExpr(matchee) match {
        case Simple(v) =>
          val binding = freshIdentifier()
          (Seq(Ast.Let(binding -> v)), Ast.Var(binding))
        case Complex(f) =>
          val binding = freshIdentifier()
          (f(Slot.store(binding)), Ast.Var(binding))
      }
      def fold(pats: List[Pat], slot: Slot): Ast = pats match {
        case Nil => Ast.Throw(Ast.Str("Match error!"))
        case Pat(_, cond, v) :: rest =>
          Ast.If(cond(matcheeVal), toBlock(v, slot), fold(rest, slot))
      }
      Complex { slot =>
        matcheeBefore ++
        (if(lets.isEmpty) Seq() else Seq(Ast.Let(lets.map(_ -> Ast.Undefined): _*))) :+
        fold(pats, slot)
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

  private def toBlock(frag: Frag, slot: Slot): Block = {
    Ast.Block(frag match {
      case Simple(ex) => Seq(slot(ex))
      case Complex(f) => f(slot)
    })
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

  def pattern(pat: Val): (Seq[String], Ast.Expr => Ast.Expr) = pat match {
    case Val.Num(n) => (Seq.empty, Ast.BinaryOp("===", _, Ast.Num(n)))
    case Val.Str(s) => (Seq.empty, Ast.BinaryOp("===", _, Ast.Str(s)))
    case S.`_` => (Seq.empty, _ => Ast.Var("true"))
    case Val.Sym(s) if s(0).isUpper =>
      (Seq(), Ast.BinaryOp("===", _, Ast.Var(s)))
    case Val.Sym(s) => (Seq(s), v => Ast.ParBlock(Seq(Ast.Assign(s, v)), Ast.Var("true")))

    case Val.Sexp(Val.Sym(constructor) :: pats) =>
      val fieldLets = pats.indices.map(_ => freshIdentifier())
      val subPats = pats.map(pattern)
      val allLets = fieldLets ++ subPats.flatMap(_._1)

      val condition = (value: Ast.Expr) => {
        val assigns = fieldLets.zipWithIndex.map {
          case (bind, i) => Ast.Assign(bind, Ast.Select(value, s"_$i"))
        }
        val self: Ast.Expr = Ast.Ternary(
          Ast.BinaryOp("===", Ast.Select(value, "tag"), Ast.Str(constructor)),
          Ast.ParBlock(assigns, Ast.Var("true")),
          Ast.Var("false")
        )
        val others = (subPats zip fieldLets).map {
          case ((_, c), b) => c(Ast.Var(b))
        }

        others.foldLeft(self)((a, b) => Ast.BinaryOp("&&", a, b))
      }
      (allLets, condition)
  }

}
