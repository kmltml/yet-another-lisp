import eval.Val
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

}
