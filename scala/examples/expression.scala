enum Expr[A]:
  case Const(value: Double)
  case Var(name: String)
  case Exp(x: A)
  case Plus(x: A, y: A)
  case Times(x: A, y: A)
import Expr.*

given Functor[Expr] with
  extension [A](e: Expr[A])
    def map[B](f: A => B): Expr[B] = e match
      case Const(v) => Const(v)
      case Var(n) => Var(n)
      case Exp(x, y) => Exp(f(x))
      case Plus(x, y) => Plus(f(x), f(y))
      case Times(x, y) => Times(f(x), f(y))

def str_expr = cata[Expr, String]{
  case Const(v) => v.toString
  case Var(name) => name
  case Exp(x) => s"exp($x)"
  case Plus(x, y) => s"($x + $y)"
  case Times(x, y) => s"($x * $y)"
}

def diff = para[Expr, Expr]((fa, o) => fa match
  case Const(v) => Const(0)
  case Var(n) => Const(1)
  case Exp(x) => Times(x, Exp(o.x))
  case Plus(x, y) => Plus(x, y)
  case Times(x, y) => Plus(Times(o.x, y), Times(x, o.y))
)

def eval(values: Map[String, Double]): Expr[Double] => Double = {
  case Const(v) => v
  case Var(n) => values(n)
  case Exp(x) => Math.exp(x)
  case Plus(x, y) => x + y
  case Times(x, y) => x * y
}

def graphviz_expr = para[Expr, String]{
  case Const(v) => v.toString
  case Var(name) => name
  case Exp((x, _)) => s"exp($x)"
  case Plus((x, _), (y, _)) => s"($x + $y)"
  case Times((x, _), (y, _)) => s"($x * $y)"
}

extension (x: Object)
  def strHash = java.lang.Integer.toString(x.hashCode, 36).replace("-", "m")

def id_alg: Expr[String] => String = {
  case Const(v) => "Const_" + v.toString.replace("-", "M").replace(".", "D")
  case Var(name) => "Var_" + name
  case Exp(x) => s"Exp_${x.strHash}"
  case Plus(x, y) => s"Plus_${(x + "_" + y).strHash}"
  case Times(x, y) => s"Times_${(x + "_" + y).strHash}"
}

def draw(ends: Boolean, step_1: Boolean) = pre_zygo[Expr, List[String], String](id_alg, (fa, id) => fa match {
    case Const(v) => List(s"$id [label=\"${v}\"${if ends then " color=red" else ""}]")
    case Var(name) => List(s"$id [label=\"${name}\"${if ends then " color=red" else ""}]")
    case Exp((xl, xid)) => xl ++ List(
      s"$id [label=\"Exp\"${if step_1 && xl.length == 1 then " color=red" else ""}]",
      s"$xid -> $id")
    case Plus((xl, xid), (yl, yid)) => xl ++ yl ++ List(
      s"$id [label=\"Plus\"${if step_1 && Math.max(xl.length, yl.length) == 1 then " color=red" else ""}]",
      s"$xid -> $id", s"$yid -> $id")
    case Times((xl, xid), (yl, yid)) => xl ++ yl ++ List(
      s"$id [label=\"Times\"${if step_1 && Math.max(xl.length, yl.length) == 1 then " color=red" else ""}]",
      s"$xid -> $id", s"$yid -> $id")
})

val const_like = raw"(\d*(?:\.\d+)?)".r
val var_like = raw"([a-zA-Z]+(?:_[a-zA-Z]+)*)".r
val bracketed = raw"\(?(.+?)\)?"
val capture_binop = (op: String) => (bracketed + raw"\s*" + op + raw"\s*" + bracketed).r
val exp_like = (raw"(?:exp|e\^)" + bracketed).r
val plus_like = capture_binop(raw"\+")
val times_like = capture_binop(raw"\*")

def parse = ana[Expr, String]{
  case plus_like(l, r) => Plus(l, r)
  case times_like(l, r) => Times(l, r)
  case exp_like(e) => Exp(e)
  case const_like(v) => Const(v.toDouble)
  case var_like(name) => Var(name)
}
