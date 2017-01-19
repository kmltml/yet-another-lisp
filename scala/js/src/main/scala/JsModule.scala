import eval.{ModuleContext, Val}

import scala.scalajs.js.Dynamic

object JsModule {

  private val defs: Map[String, Val] = Map(
    "global" -> Val.Builtin {
      case List(Val.Sym(s)) => jsToVal(Dynamic.global.selectDynamic(s))
    },
    "prop" -> Val.Builtin {
      case List(Val.Sym(prop), Val.Native(obj)) =>
        jsToVal(obj.asInstanceOf[Dynamic].selectDynamic(prop).asInstanceOf[Any])
    },
    "meth" -> Val.Builtin {
      case Val.Sym(meth) :: Val.Native(obj) :: args =>
        jsToVal(obj.asInstanceOf[Dynamic].applyDynamic(meth)(args.map(valToJs(_).asInstanceOf[scalajs.js.Any]): _*))
    }
  )

  def valToJs(v: Val): Any = v match {
    case Val.Num(n) => n
    case Val.Str(s) => s
    case Val.Native(v) => v
  }

  def jsToVal(x: Any): Val = x match {
    case s: String => Val.Str(s)
    case n: Double => Val.Num(n)
    case f if scala.scalajs.js.typeOf(f.asInstanceOf[scala.scalajs.js.Any]) == "function" =>
      Val.Builtin(args => jsToVal(f.asInstanceOf[scala.scalajs.js.Function].call(null, args.map(valToJs(_).asInstanceOf[scalajs.js.Any]): _*)))
    case x => Val.Native(x)
  }

  val module = new ModuleContext(defs.map { case (k, v) => Val.Sym(k) -> v }, Seq(Val.Sym("prelude")))

}
