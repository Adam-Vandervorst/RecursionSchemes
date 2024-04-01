package examples.multiexpression

import be.adamv.recursionschemes.{const_monoid_app, *, given}


enum Expr:
  case Con(i: Int)
  case Add(x: Expr, y: Expr)
  case Mul(x: Expr, y: Expr)
  case EVar(name: String)
  case Let(decl: Decl, body: Expr)
export Expr.*
enum Decl:
  case `:=`(name: String, value: Expr)
  case `;`(l: Decl, r: Decl)
export Decl.*

case class Plate[F[_]](expr: Expr => F[Expr], decl: Decl => F[Decl])

given MultiPlate[Plate] with
  override def multiplate[F[_] : Applicative](child: => Plate[F]): Plate[F] =
    Plate({
      case Add(e1, e2) => Add.apply.curried.pure app child.expr(e1) app child.expr(e2)
      case Mul(e1, e2) => Mul.apply.curried.pure app child.expr(e1) app child.expr(e2)
      case Let(d, e) => Let.apply.curried.pure app child.decl(d) app child.expr(e)
      case e => e.pure
    }, {
      case `:=`(v, e) => `:=`.apply.curried.pure app v.pure app child.expr(e)
      case `;`(d1, d2) => `;`.apply.curried.pure app child.decl(d1) app child.decl(d2)
    })

  override def mkPlate[F[_]](build: [A] => Projector[Plate, A][F] => A => F[A]): Plate[F] =
    Plate(build(_.expr), build(_.decl))

val getVariablesPlate: Plate[[X] =>> List[String]] =
  purePlate(summon[MultiPlate[Plate]], const_monoid_app).copy(expr = {
    case EVar(v) => v::Nil
    case x => Nil
  })

val variablesPlate = preorderFold(getVariablesPlate)

val doConstFold: Plate[Box] =
  purePlate(summon[MultiPlate[Plate]], monad_app[Box]).copy(expr = {
    case Add(Con(x), Con(y)) => Box(Con(x + y))
    case Mul(Con(x), Con(y)) => Box(Con(x * y))
    case x => Box(x)
  })

val constFoldPlate: Plate[Box] = mapFamilyM(doConstFold)
