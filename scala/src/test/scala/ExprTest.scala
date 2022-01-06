import examples.expression.{*, given}
import lib.{*, given}
import org.scalatest.funsuite.AnyFunSuite

class ExprTest extends AnyFunSuite:
  import Expr.*

  val x = Fix(Var("x"))
  val y = Fix(Var("y"))
  val z = Fix(Var("z"))
  val x2 = Fix(Times(x, x))
  val y2 = Fix(Times(y, y))

  val e1 = Fix(Plus(Fix(Const(1)), Fix(Const(2))))
  val e2 = Fix(Plus(e1, Fix(Const(3))))
  val e3 = Fix(Times(Fix(Times(e2, Fix(Exp(x)))), y))
  val e4 = Fix(Plus(Fix(Plus(x2, Fix(Times(x, y)))), y2))

  val e5s =  "(exp(10.0) + ((x * (x * 1.0)) + (y + z)))"
  val e5 = Fix(Plus(Fix(Exp(Fix(Const(10.0)))),Fix(Plus(Fix(Times(x,Fix(Times(x,Fix(Const(1.0)))))),Fix(Plus(y,z))))))

  test("pretty"){
    assert(pretty(e3) == "(1.0 + 2.0 + 3.0)*e^x*y")
    assert(pretty(e4) == "x*x + x*y + y*y")
  }

  test("diff pretty"){
    assert(pretty(Fix(diff("x")(e3))) == "(1.0 + 2.0 + 3.0)*e^x*0.0 + y*((1.0 + 2.0 + 3.0)*1.0*e^x + e^x*(0.0 + 0.0 + 0.0))")
    assert(pretty(Fix(diff("x")(e4))) == "x*1.0 + x*1.0 + x*0.0 + y*1.0 + y*0.0 + y*0.0")
  }

  test("eval"){
    assert(eval(Map("x" -> Math.PI, "y" -> 1d/3))(e3) == 46.281385265558534)
    assert(eval(Map("x" -> 2, "y" -> 3))(e4) == 19)
  }

  test("parse"){
    assert(parse(e5s) == e5)
  }

  test("string"){
    assert(string(e5) == e5s)
  }

  test("parse string"){
    assert(parse(string(e3)) == e3)
    assert(parse(string(e4)) == e4)
    assert(parse(string(e5)) == e5)
  }