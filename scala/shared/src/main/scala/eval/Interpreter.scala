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
    "<=" -> { case List(Val.Num(a), Val.Num(b)) => if(a <= b) S.`true` else S.`false` },
    ">=" -> { case List(Val.Num(a), Val.Num(b)) => if(a >= b) S.`true` else S.`false` }
  )

  val prelude: ModuleContext = new ModuleContext(
    builtinFunctions.map{ case (k, v) => Val.Sym(k) -> Val.Builtin(v) },
    imports = Seq.empty
  )

  private def binOp(f: (Double, Double) => Double)(args: List[Val]): Val = {
    args match {
      case Val.Num(v) :: Nil =>
        Val.Builtin(a => Val.Num(a.map{ case Val.Num(n) => n }.foldLeft(v)(f)))
      case _ :: _ :: _ => // at least two elements
        Val.Num(args.map{ case Val.Num(n) => n }.reduce(f))
    }
  }

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
      case Val.Sexp(S.`def` :: (name: Val.Sym) :: value :: Nil) =>
        name -> eval(value, ctxt)(global).value // No recursion taking place here, so it's fine to escape eval context
      case Val.Sexp(S.`def` :: d) =>
        val pairs = d.grouped(2).toList
        val Val.Sexp((name: Val.Sym) :: _) :: _ = pairs.head
        val bodies = pairs map {
          case Seq(Val.Sexp(`name` :: patterns), body) =>
            patterns -> body
        }
        name -> Val.Def(bodies, ctxt)
    }
    val lastExp = program.lastOption.filter {
      case Val.Sexp(S.`def` :: _) => false
      case _ => true
    }
    val moduleContext = new ModuleContext((defs ++ dataDefs).toMap, imports = Seq(Val.Sym("prelude")))
    val newGlobal = new Global(global.moduleContexts + (module -> moduleContext))
    (newGlobal, lastExp.map(eval(_, ctxt)(newGlobal).value).getOrElse(S.nil))

  }

  def eval(v: Val, ctxt: Context)(implicit global: Global): Eval[Val] = v match {
    case _: Val.Num | _: Val.Str => Eval.now(v)

    case s: Val.Sym =>
      val x = ctxt(s)
      .orElse(global.moduleContexts(ctxt.module).defs.get(s))
      .orElse(global.moduleContexts(ctxt.module).imports.view
        .map(global.moduleContexts(_).defs.get(s))
        .collectFirst {
          case Some(v) => v
        }).get
      Eval.now(x)

    case Val.Sexp(fn :: args) => prefix(ctxt, fn, args)
  }

  def prefix(ctxt: Context, fn: Val, args: List[Val])(implicit global: Global): Eval[Val] = fn match {
    case S.`if` =>
      val List(cond, t, f) = args
      eval(cond, ctxt) flatMap {
        case S.`true` => eval(t, ctxt)
        case _ => eval(f, ctxt)
      }

    case S.let =>
      val List(Val.Sexp(lets), body) = args
      val newCtxt = lets.foldLeft(Eval.now(ctxt)) {
        case (c, Val.Sexp(List((name: Val.Sym), value))) =>
          for {
            context <- c
            v <- eval(value, context)
          } yield context + (name -> v)
      }
      newCtxt.flatMap(eval(body, _))

    case S.lambda | S.λ =>
      val List(Val.Sexp(a), body) = args
      Eval.now(Val.Lambda(a.map { case s: Val.Sym => s }, body, ctxt))

    case S.`match` =>
      val v :: patterns = args
      eval(v, ctxt).flatMap { matchee =>
        val x = patterns.traverseU {
          case Val.Sexp(List(pattern, body)) =>
            patmat(ctxt, matchee, pattern).flatMap { res =>
              res.map { binds =>
                eval(body, ctxt ++ binds)
              } match {
                case Some(e) => e.map(Option(_))
                case None => Eval.now(Option.empty[Val])
              }
            }
        }
        x.map(_.collectFirst {
          case Some(v) => v
        }.getOrElse(throw new AssertionError("Match error!")))
      }

    case _ =>
      eval(fn, ctxt).flatMap {
        case fn: Val.Fun =>
          for {
            args <- args.traverseU(eval(_, ctxt))
            v <- applyFun(fn, args, ctxt)
          } yield v
      }

  }

  def applyFun(fn: Val.Fun, args: List[Val], ctxt: Context)(implicit global: Global): Eval[Val] = {
    fn match {
      case Val.Builtin(f) => Eval.now(f(args))
      case Val.Lambda(argNames, body, c) =>
        if(argNames.size > args.size) {
          Eval.now(Val.Builtin { a => applyFun(fn, args ++ a, ctxt).value }) // TODO Should `Builtin#run`'s type be changed to List[Val] => Eval[Val] ?
        } else {
          val bindings = argNames zip args
          val innerContext = c ++ bindings
          eval(body, innerContext)
        }
      case d @ Val.Def(bodies, bodyContext) =>
        if(d.numParams > args.size) {
          Eval.now(Val.Builtin { a => applyFun(fn, args ++ a, ctxt).value })
        } else {
          bodies.foldLeft(Eval.now(None: Option[Val])) { case (e, (patterns, body)) =>
            e.flatMap {
              case Some(_) => e
              case None =>
                (patterns zip args).traverseU {
                  case (pat, v) => patmat(ctxt, v, pat)
                }.flatMap { results =>
                  val binds = results.sequenceU.map(_.foldLeft(Map.empty[Val.Sym, Val])(_ ++ _))
                  binds match {
                    case Some(b) => Eval.defer(eval(body, bodyContext ++ b)).map(Option(_))
                    case None => Eval.now(None: Option[Val])
                  }
                }
            }
          }.map(_.getOrElse(throw new AssertionError(s"Match error: Can't match $args to any of ${ bodies.map(_._1) }")))
        }
    }
  }

  def patmat(ctxt: Context, matchee: Val, pattern: Val)(implicit global: Global): Eval[Option[Map[Val.Sym, Val]]] = pattern match {
    case Val.Num(v) =>
      Eval.now(matchee match {
        case Val.Num(`v`) => Some(Map.empty)
        case _ => None
      })

    case Val.Str(v) =>
      Eval.now(matchee match {
        case Val.Str(`v`) => Some(Map.empty)
        case _ => None
      })

    case S.`_` => Eval.now(Some(Map.empty))

    case s @ Val.Sym(name) if name(0).isUpper =>
      eval(s, ctxt).map {
        case x if x == matchee => Some(Map.empty)
        case _ => None
      }

    case s: Val.Sym =>
      Eval.now(Some(Map(s -> matchee)))

    case Val.Sexp((constr: Val.Sym) :: args) =>
      matchee match {
        case Val.Data(`constr`, members) =>
          assert(members.length == args.length, "Wrong number of parameters in adt pattern!")
          (args zip members).traverseU {
            case (p, v) => patmat(ctxt, v, p)
          }.map(_.sequenceU.map(_.reduce(_ ++ _)))

        case _ => Eval.now(None)
      }
  }

}
