import eval.Val
import fastparse.core.Parsed.Success
import js.{Compiler, Printer, Simple}
import parsing.Parser

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExportAll
@JSExport
object CompilerInterface {

  def compile(source: String): String = {
    val Success(Seq(sval), _) = Parser.program.parse(source)
    val prog = Val.desugar(sval)
    val Simple(ast) = Compiler.compileExpr(prog)
    Printer.print(ast)
  }

}
