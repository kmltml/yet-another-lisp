package js

import eval.Val
import fastparse.core.Parsed.Success
import parsing.Parser
import utest._

object IntegrationTests extends TestSuite {

  def exec(source: String): Any = {
    val Success(svals, _) = Parser.program.parse(source)
    val prog = svals.map(Val.desugar)
    val asts = prog.flatMap(Compiler.compileStatement)
    val jsOut = Printer.print(Ast.Block(asts))
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
    "simple def and its usage" - {
      exec(
        """
          |(def (foo x) (+ x 1))
          |
          |(foo 2)
        """.stripMargin) ==> 3
    }
    "a simple if statement" - {
      exec("(if true 1 0)") ==> 1
      exec("(if false 1 0)") ==> 0
    }
    "simple pattern match on numbers" - {
      exec(
        """
          |(match (+ 2 3)
          |  (1 "one")
          |  (2 "two")
          |  (3 "three")
          |  (4 "four")
          |  (5 "five")
          |  (_ "much"))
        """.stripMargin) ==> "five"
    }
    "Simple adt declaration and usage" - {
      val res = exec(
        """
          |(data (Option a)
          |  (Some a)
          |  (None))
          |
          |(Some None)
        """.stripMargin).asInstanceOf[scalajs.js.Dynamic]
      res.tag ==> "Some"
      res._0.tag ==> "None"
    }
  }

}
