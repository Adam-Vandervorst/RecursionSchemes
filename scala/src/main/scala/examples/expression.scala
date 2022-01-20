package examples.expression

import lib.{*, given}

import scala.annotation.tailrec

enum Expr[A]:
  case Const(value: Double)
  case Var(name: String)
  case Exp(x: A)
  case Plus(x: A, y: A)
  case Times(x: A, y: A)
import Expr.*

given Traversable[Expr] with
  extension [A](e: Expr[A])
    def map[B](f: A => B): Expr[B] = e match
      case Const(v) => Const(v)
      case Var(n) => Var(n)
      case Exp(x) => Exp(f(x))
      case Plus(x, y) => Plus(f(x), f(y))
      case Times(x, y) => Times(f(x), f(y))

    def traverse[F[_], B](f: A => F[B])(using AF: Applicative[F]): F[Expr[B]] = e match
      case Const(v) => AF.pure(Const(v))
      case Var(n) => AF.pure(Var(n))
      case Exp(x) => AF.pure(Exp[B].apply).app(f(x))
      case Plus(x, y) => AF.pure(Plus[B].apply.curried).app(f(x)).app(f(y))
      case Times(x, y) => AF.pure(Times[B].apply.curried).app(f(x)).app(f(y))

def pretty = para[Expr, String]{
  case Const(v) => v.toString
  case Var(name) => name
  case Exp((x, Fix(_: Var[_]))) => s"e^$x"
  case Exp((x, _)) => s"exp($x)"
  case Times((x, Fix(_: Plus[_])), (y, Fix(_: Plus[_]))) => s"($x)*($y)"
  case Times((x, _), (y, Fix(_: Plus[_]))) => s"$x*($y)"
  case Times((x, Fix(_: Plus[_])), (y, _)) => s"($x)*$y"
  case Times((x, _), (y, _)) => s"$x*$y"
  case Plus((x, _), (y, _)) => s"$x + $y"
}

def diff(to: String) = para[Expr, Expr[Fix[Expr]]]{
  case Const(_) => Const(0)
  case Var(`to`) => Const(1)
  case Var(_) => Const(0)
  case Exp((x, o)) => Times(Fix(x), Fix(Exp(o)))
  case Plus((x, _), (y, _)) => Plus(Fix(x), Fix(y))
  case Times((x, ox), (y, oy)) => Plus(Fix(Times(ox, Fix(y))), Fix(Times(oy, Fix(x))))
}

def eval(values: Map[String, Double]) = cata[Expr, Double]{
  case Const(v) => v
  case Var(n) => values(n)
  case Exp(x) => Math.exp(x)
  case Plus(x, y) => x + y
  case Times(x, y) => x * y
}

val const_like = raw"(\d+(?:\.\d+)?)".r
val var_like = raw"([a-zA-Z]+(?:_[a-zA-Z]+)*)".r
val exp_like = raw"exp\((.+)\)".r

def maybe_parse = anaM[Expr, Option, String]{
  case exp_like(e) => Some(Exp(e))
  case const_like(v) => Some(Const(v.toDouble))
  case var_like(name) => Some(Var(name))
  case balanced.unlift(bs) =>
    val s = bs.drop(1).dropRight(1)
    val op_positions = Seq("+", "*").map(s.indexOf).filter(_ > 0)
    if op_positions.isEmpty then None
    else
      val first_op = op_positions.min - 1
      val (l, tail) = balanced(s).fold(s.splitAt(first_op))(h => (h, s.stripPrefix(h)))
      val (op, r) = (tail(1), tail.drop(3))
      Some(if op == '+' then Plus(l, r) else Times(l, r))
  case _ => None
}

def string = cata[Expr, String]{
  case Const(v) => v.toString
  case Var(name) => name
  case Exp(x) => s"exp($x)"
  case Plus(x, y) => s"($x + $y)"
  case Times(x, y) => s"($x * $y)"
}

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
