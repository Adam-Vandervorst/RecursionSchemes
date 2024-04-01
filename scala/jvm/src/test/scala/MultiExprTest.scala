import examples.multiexpression.{*, given}
import be.adamv.recursionschemes.{*, given}
import org.scalatest.funsuite.AnyFunSuite

class MultiExprTest extends AnyFunSuite:
  val e1 = Add(Mul(Con(42), Con(68)), Con(7))
  val d1 = :=("x", e1)
  val d2 = `;`(:=("p", Mul(EVar("x"), EVar("y"))),
               :=("r", Mul(EVar("a"), EVar("b"))))

  test("variables") {
    assert(variablesPlate.expr(e1) == List())
    assert(variablesPlate.decl(d1) == List())
    assert(variablesPlate.decl(d2) == List("x", "y", "a", "b"))
  }

  test("constant folding") {
    val constantFoldE: Expr => Expr = traverseFor[Plate, Expr](_.expr)(constFoldPlate)
    val constantFoldD: Decl => Decl = traverseFor[Plate, Decl](_.decl)(constFoldPlate)
    assert(constantFoldE(e1) == Add(Con(2856),Con(7)))
    assert(constantFoldD(d1) == :=("x",Add(Con(2856),Con(7))))
    assert(constantFoldD(d2) == d2)
  }
