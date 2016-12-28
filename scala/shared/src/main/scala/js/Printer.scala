package js

object Printer {

  def print(ast: Ast): String = ast match {
    case Ast.Num(n) => n.toString
    case Ast.Undefined => "undefined"
    case Ast.Assign(b, v) => s"$b = ${print(v)};"
    case Ast.Let(bs @ _*) =>
      val bindings = bs.map {
        case (name, Ast.Undefined) => name
        case (name, value) => s"$name = ${print(value)}"
      }
      s"let ${bindings.mkString(", ")};"
    case Ast.Block(stmnts) =>
      s"{${ stmnts.map(print).mkString }}"
    case Ast.Var(name) => name
    case Ast.Lambda(params, body) =>
      val lhs = params match {
        case Seq()  => "()"
        case Seq(a) => s"$a"
        case _ => s"(${params.mkString(", ")})"
      }
      s"($lhs => ${print(body)})"
    case Ast.Apply(f, a) =>
      s"${print(f)}(${a.map(print).mkString(",")})"
    case Ast.BinaryOp(op, a, b) => s"(${print(a)} $op ${print(b)})"
  }

}
