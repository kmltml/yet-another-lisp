import eval.{Global, Interpreter, Val}
import fastparse.core.Parsed.Success
import js.{Ast, Compiler, Printer, Simple}
import parsing.Parser

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExportAll
@JSExport
object CompilerInterface {

  def compile(source: String): String = {
    val Success(svals, _) = Parser.program.parse(source)
    val prog = svals.map(Val.desugar)
    val asts = prog.flatMap(Compiler.compileStatement)
    Printer.print(Ast.Block(asts))
  }

  def prettyPrint(v: Val): String = v match {
    case Val.Sym(s) => s"'$s"
    case Val.Sexp(ss) => s"(${ ss.map(prettyPrint).mkString(" ") })"
    case Val.Builtin(_) => s"__fun__"
    case Val.Lambda(_, _, _) => s"__lambda__"
    case Val.Data(Val.Sym(c), Nil) => s"$c"
    case Val.Data(Val.Sym(c), members) => s"$c(${ members.map(prettyPrint).mkString(" ") })"
    case Val.Num(v) => v.toString
    case Val.Str(v) => '"' + v + '"'
    case Val.Native(v) => v.toString
  }

  def eval(source: String): String = {
    val Success(svals, _) = Parser.program.parse(source)
    val prog = svals.map(Val.desugar)
    val global = new Global(Map(
      Val.Sym("prelude") -> Interpreter.prelude,
      Val.Sym("js") -> JsModule.module
    ))
    prettyPrint(Interpreter.evalProgram(prog.toList, global)._2)
  }

}
