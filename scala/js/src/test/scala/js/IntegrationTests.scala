package js

import eval.Val
import fastparse.core.Parsed.Success
import parsing.Parser
import utest._

object IntegrationTests extends TestSuite {

  def exec(source: String): Any = {
    // TODO handle multi-statement programs
    val Success(Seq(sval), _) = Parser.program.parse(source)
    val prog = Val.desugar(sval)
    val Simple(ast) = Compiler.compileExpr(prog)
    val jsOut = Printer.print(ast)
    println("Compiled javascript output:")
    println(jsOut)
    println("===========================")
    scalajs.js.eval(jsOut)
  }

  val tests = apply {
    "number addition" - {
      exec("(+ 2 3)") ==> 5
    }
    "mixed priority number operators" - {
      exec("(* (+ 2 3) 4)") ==> 20
    }
    "lambda abstraction and application" - {
      exec("((λ (x y) (+ x y)) 1 2)") ==> 3
    }
  }

}
