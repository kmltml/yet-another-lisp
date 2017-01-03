package js

object Printer {

  def print(ast: Ast): String = ast match {
    case Ast.Num(n) => n.toString
    case Ast.Str(s) => printStringLiteral(s)

    case Ast.Undefined => "undefined"
    case Ast.Obj(props @ _*) =>
      props.map {
        case (k, v) => s"${printStringLiteral(k)}: ${print(v)}"
      }.mkString("({", ",", "})")
    case Ast.Assign(b, v) => s"$b = ${print(v)}"
    case Ast.Let(bs @ _*) =>
      val bindings = bs.map {
        case (name, Ast.Undefined) => name
        case (name, value) => s"$name = ${print(value)}"
      }
      s"let ${bindings.mkString(", ")};"
    case Ast.If(c, t, f) =>
      val iff = s"if(${print(c)}) ${print(t)}"
      val els = f match {
        case Ast.Empty => ""
        case _ => s"else ${print(f)}"
      }
      s"$iff $els"
    case Ast.Block(stmnts) =>
      s"{${ stmnts.map(print).mkString }}"
    case Ast.ParBlock(asts, e) =>
      s"(${asts.map(print).mkString(",")}, ${print(e)})"
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
    case Ast.Return(v) =>
      s"return ${print(v)};"
    case Ast.Throw(v) =>
      s"throw ${print(v)};"
    case Ast.Select(o, p) => s"${print(o)}.$p"
    case Ast.Ternary(c, t, f) =>
      s"(${print(c)}?${print(t)}:${print(f)})"
    case Ast.BinaryOp(op, a, b) => s"(${print(a)} $op ${print(b)})"
  }

  def printStringLiteral(s: String): String = {
    val escaped = s.flatMap {
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\\' => "\\\\"
      case c => c.toString
    }
    '"' + escaped + '"'
  }

}
