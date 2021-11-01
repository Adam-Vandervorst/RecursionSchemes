enum ExprF[A]:
  case ConstF(value: Double)
  case VarF(name: String)
  case ExpF(x: A)
  case PlusF(x: A, y: A)
  case TimesF(x: A, y: A)
import ExprF.*

given Functor[ExprF] with
  extension [A](e: ExprF[A])
    def map[B](f: A => B): ExprF[B] = e match
      case ConstF(v) => ConstF(v)
      case VarF(n) => VarF(n)
      case ExpF(x, y) => ExpF(f(x))
      case PlusF(x, y) => PlusF(f(x), f(y))
      case TimesF(x, y) => TimesF(f(x), f(y))

def str_expr = cata[ExprF, String]{
  case ConstF(v) => v.toString
  case VarF(name) => name
  case ExpF(x) => s"exp($x)"
  case PlusF(x, y) => s"($x + $y)"
  case TimesF(x, y) => s"($x * $y)"
}

def diff = para[ExprF, ExprF]((fa, o) => fa match
  case ConstF(v) => ConstF(0)
  case VarF(n) => ConstF(1)
  case ExpF(x) => TimesF(x, ExpF(o.x))
  case PlusF(x, y) => PlusF(x, y)
  case TimesF(x, y) => PlusF(Times(o.x, y), Times(x, o.y))
)

def eval(values: Map[String, Double]): ExprF[Double] => Double = {
  case ConstF(v) => v
  case VarF(n) => values(n)
  case ExpF(x) => Math.exp(x)
  case PlusF(x, y) => x + y
  case TimesF(x, y) => x * y
}

def graphviz_expr = para[ExprF, String]{
  case ConstF(v) => v.toString
  case VarF(name) => name
  case ExpF((x, _)) => s"exp($x)"
  case PlusF((x, _), (y, _)) => s"($x + $y)"
  case TimesF((x, _), (y, _)) => s"($x * $y)"
}

extension (x: Object)
  def strHash = java.lang.Integer.toString(x.hashCode, 36).replace("-", "m")

def id_alg: ExprF[String] => String = {
  case ConstF(v) => "Const_" + v.toString.replace("-", "M").replace(".", "D")
  case VarF(name) => "Var_" + name
  case ExpF(x) => s"Exp_${x.strHash}"
  case PlusF(x, y) => s"Plus_${(x + "_" + y).strHash}"
  case TimesF(x, y) => s"Times_${(x + "_" + y).strHash}"
}

def draw(ends: Boolean, step_1: Boolean) = weak_zygo[ExprF, List[String], String](id_alg, (fa, id) => fa match {
    case ConstF(v) => List(s"$id [label=\"${v}\"${if ends then " color=red" else ""}]")
    case VarF(name) => List(s"$id [label=\"${name}\"${if ends then " color=red" else ""}]")
    case ExpF((xl, xid)) => xl ++ List(
      s"$id [label=\"Exp\"${if step_1 && xl.length == 1 then " color=red" else ""}]",
      s"$xid -> $id")
    case PlusF((xl, xid), (yl, yid)) => xl ++ yl ++ List(
      s"$id [label=\"Plus\"${if step_1 && Math.max(xl.length, yl.length) == 1 then " color=red" else ""}]",
      s"$xid -> $id", s"$yid -> $id")
    case TimesF((xl, xid), (yl, yid)) => xl ++ yl ++ List(
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

def parse = ana[ExprF, String]{
  case plus_like(l, r) => PlusF(l, r)
  case times_like(l, r) => TimesF(l, r)
  case exp_like(e) => ExpF(e)
  case const_like(v) => ConstF(v.toDouble)
  case var_like(name) => VarF(name)
}
