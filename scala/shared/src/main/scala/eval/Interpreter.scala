package eval

import cats._
import cats.implicits._

object Interpreter {

  def builtinFunctions: Map[String, List[Val] => Val] = Map(
    "+" -> binOp(_ + _),
    "-" -> binOp(_ - _),
    "*" -> binOp(_ * _),
    "/" -> binOp(_ / _),
    "=" -> { case List(a, b) => if(a == b) S.`true` else S.`false` },
    ">" -> { case List(Val.Num(a), Val.Num(b)) => if(a > b) S.`true` else S.`false` },
    "<" -> { case List(Val.Num(a), Val.Num(b)) => if(a < b) S.`true` else S.`false` },
    "<=" -> { case List(Val.Num(a), Val.Num(b)) => if(a >= b) S.`true` else S.`false` },
    ">=" -> { case List(Val.Num(a), Val.Num(b)) => if(a <= b) S.`true` else S.`false` }
  )

  val prelude: ModuleContext = new ModuleContext(
    builtinFunctions.map{ case (k, v) => Val.Sym(k) -> Val.Builtin(v) },
    imports = Seq.empty
  )

  private def binOp(f: (Double, Double) => Double)(args: List[Val]): Val =
    Val.Num(args.map{ case Val.Num(n) => n }.reduce(f))

//  def defaultContext: Context = Context(
//    builtinFunctions.map{ case (k, v) => Val.Sym(k) -> Val.Builtin(v) },
//
//  )

  def defaultContext: Context = Context.empty(Val.Sym("default"))

  def defaultGlobal: Global = new Global(Map(Val.Sym("prelude") -> prelude, Val.Sym("default") -> new ModuleContext(Map.empty, Seq(Val.Sym("prelude")))))

  def evalProgram(program: List[Val], global: Global = defaultGlobal): (Global, Val) = {
    val module = Val.Sym("default")
    val ctxt = Context.empty(module)

    val dataDefs: Seq[(Val.Sym, Val)] = program.collect {
      case Val.Sexp(S.data :: Val.Sexp(_) :: cs) =>
        cs.map {
          case Val.Sexp((name: Val.Sym) :: Nil) =>
            name -> Val.Data(name, Nil)
          case Val.Sexp((name: Val.Sym) :: args) =>
            name -> Val.Builtin { as =>
              assert(args.length == as.length)
              Val.Data(name, as)
            }
        }
    }.flatten

    val defs: Seq[(Val.Sym, Val)] = program.collect {
      case Val.Sexp(S.`def` :: d) =>
        val pairs = d.grouped(2).toList
        val Val.Sexp((name: Val.Sym) :: _) :: _ = pairs.head
        val bodies = pairs map {
          case Seq(Val.Sexp(`name` :: patterns), body) =>
            patterns -> body
        }
        name -> Val.Def(bodies)
    }
    val lastExp = program.lastOption.filter {
      case Val.Sexp(S.`def` :: _) => false
      case _ => true
    }
    val moduleContext = new ModuleContext((defs ++ dataDefs).toMap, imports = Seq(Val.Sym("prelude")))
    val newGlobal = new Global(global.moduleContexts + (module -> moduleContext))
    (newGlobal, lastExp.map(eval(_, ctxt)(newGlobal)).getOrElse(S.nil))

  }

  def eval(v: Val, ctxt: Context)(implicit global: Global): Val = v match {
    case _: Val.Num | _: Val.Str => v

    case s: Val.Sym => ctxt(s)
      .orElse(global.moduleContexts(ctxt.module).defs.get(s))
      .orElse(global.moduleContexts(ctxt.module).imports.view
        .map(global.moduleContexts(_).defs.get(s))
        .collectFirst {
          case Some(v) => v
        }).get

    case Val.Sexp(fn :: args) => apply(ctxt, fn, args)
  }

  def apply(ctxt: Context, fn: Val, args: List[Val])(implicit global: Global): Val = fn match {
    case S.`if` =>
      val List(cond, t, f) = args
      eval(cond, ctxt) match {
        case S.`true` => eval(t, ctxt)
        case _ => eval(f, ctxt)
      }
    case S.lambda | S.λ =>
      val List(Val.Sexp(a), body) = args
      Val.Lambda(a.map { case s: Val.Sym => s }, body, ctxt)

    case S.`match` =>
      val v :: patterns = args
      val matchee = eval(v, ctxt)

      patterns.view.map {
        case Val.Sexp(List(pattern, body)) =>
          val res = patmat(ctxt, matchee, pattern)
          res.map { binds =>
            eval(body, ctxt ++ binds)
          }
      }.collectFirst {
        case Some(v) => v
      }.getOrElse(throw new AssertionError("Match error!"))

    case _ => eval(fn, ctxt) match {
      case Val.Builtin(f) => f(args.map(eval(_, ctxt)))
      case Val.Lambda(argNames, body, c) =>
        val bindings = argNames zip args.map(eval(_, ctxt))
        val innerContext = c ++ bindings
        eval(body, innerContext)
      case Val.Def(bodies) =>
        val argValues = args.map(eval(_, ctxt))
        bodies.view.map {
          case (patterns, body) =>
            val results = (patterns zip argValues).traverseU {
              case (pat, v) => patmat(ctxt, v, pat)
            }
            val binds = results.map(_.foldLeft(Map.empty[Val.Sym, Val])(_ ++ _))
            binds.map(b => eval(body, ctxt ++ b))
        }.collectFirst {
          case Some(v) => v
        }.get
    }
  }

  def patmat(ctxt: Context, matchee: Val, pattern: Val)(implicit global: Global): Option[Map[Val.Sym, Val]] = pattern match {
    case Val.Num(v) =>
      matchee match {
        case Val.Num(`v`) => Some(Map.empty)
        case _ => None
      }

    case Val.Str(v) =>
      matchee match {
        case Val.Str(`v`) => Some(Map.empty)
        case _ => None
      }

    case S.`_` => Some(Map.empty)

    case s @ Val.Sym(name) if name(0).isUpper =>
      if(eval(s, ctxt) == matchee)
        Some(Map.empty)
      else
        None

    case s: Val.Sym =>
      Some(Map(s -> matchee))

    case Val.Sexp((constr: Val.Sym) :: args) =>
      matchee match {
        case Val.Data(`constr`, members) =>
          assert(members.length == args.length, "Wrong number of parameters in adt pattern!")
          val results = (args zip members).map {
            case (p, v) => patmat(ctxt, v, p)
          }
          results.foldLeft(Some(Map.empty): Option[Map[Val.Sym, Val]]) {
            case (None, _) => None
            case (Some(a), Some(b)) => Some(a ++ b)
            case (_, None) => None
          }
        case _ => None
      }
  }

}
