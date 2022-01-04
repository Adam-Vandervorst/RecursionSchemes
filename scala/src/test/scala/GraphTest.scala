import examples.algebraic_graph.{*, given}
import lib.{*, given}

import org.scalatest.funsuite.AnyFunSuite

class GraphTest extends AnyFunSuite:
  import Graph._

  val e = Empty
  val v0 = Vertex(0)
  val v1 = Vertex(1)
  val o01 = Overlay(v0, v1)
  val o1e = Overlay(v1, e)
  val c01 = Connect(v0, v1)
  val c1e = Connect(v1, e)
  val coo = Connect(o01, o1e)
  val cee = Connect(e, e)

  val fe: Fix[[X] =>> Graph[Int, X]] = Fix(e)
  val fv0 = Fix(v0)
  val fv1 = Fix(v1)
  val fo01 = Fix(Overlay(fv0, fv1))
  val fo1e = Fix(Overlay(fv1, fe))
  val fc01 = Fix(Connect(fv0, fv1))
  val fc1e = Fix(Connect(fv1, fe))
  val fcoo = Fix(Connect(fo01, fo1e))
  val fcee = Fix(Connect(fe, fe))

  val floo = Fix(Overlay(Fix(Overlay(fv0, fv1)), fv0))
  val froo = Fix(Overlay(fv0, Fix(Overlay(fv1, fv0))))

  val small_adj = Map(("D",0.1) -> Set(("C",0.5)), ("B",1d) -> Set(), ("C",0.5) -> Set(("A",0.5)), ("A",0.5) -> Set(("A",0.5), ("C",0.5)))
  val flow_g = Fix(Overlay(Fix(Vertex(("M",0.5))), Fix(Connect(Fix(Overlay(Fix(Connect(Fix(Vertex(("X",0.8))), Fix(Vertex(("Y",1.0))))), Fix(Vertex(("Z",1.0))))), Fix(Vertex(("W",0.2)))))))
  val flow_adj = Map(("W",0.2) -> Set(), ("Y",1.0) -> Set(("W",0.2)), ("M",0.5) -> Set(), ("Z",1.0) -> Set(("W",0.2)), ("X",0.8) -> Set(("W",0.2), ("Y",1.0)))

  val test_tree = node("root",
    node("A", node("AA"), node("AB"), node("AC")),
    node("B", node("BA", node("BAA")), node("BB")),
    node("C"))

  test("functor"){
    assert(e.map(_ => 1) == e)
    assert(o01.map(_ => e) == Overlay(e, e))
    assert(o1e.map{case Empty => v1; case Vertex(_) => v0; case _ => e} == o01)
    assert(coo.map(_ => e) == cee)
    assert(c1e.map(x => x.toString) == Connect(v1.toString, e.toString))
  }

  test("cata vertexSet"){
    assert(vertexSet(fe) == Set())
    assert(vertexSet(fcoo) == Set(0, 1))
  }

  test("cata toAdj") {
    assert(toAdj(fo1e) == Map(1 -> Set()))
    assert(toAdj(fcoo) == Map(0 -> Set(1), 1 -> Set(1)))
    assert(toAdj(flow_g) == flow_adj)
  }

  test("futu fromAdj"){
    assert(vertexSet(fromAdj(small_adj.toSeq)) == small_adj.keySet)
  }

  test("fromAdj toAdj"){
    assert(toAdj(fromAdj(small_adj.toSeq)) == small_adj)
    assert(toAdj(fromAdj(flow_adj.toSeq)) == flow_adj)
  }

  test("prothesi pretty"){
    assert(pretty(fcoo, Seq()) == "((0 + 1) -> (1 + -))")
    assert(pretty(flow_g, Seq()) == "((M,0.5) + ((((X,0.8) -> (Y,1.0)) + (Z,1.0)) -> (W,0.2)))")
  }

  test("pretty assoc"){
    assert(pretty(floo, Seq()) == pretty(froo, Seq()))
  }
  
  test("path pretty"){
    assert(pretty(path(Seq(1, 2, 3, 4)), Seq()) == "((1 -> 2) + (2 -> 3) + (3 -> 4))")
    assert(pretty(path(Seq(1, 2, 3, 4, 5)), Seq()) == "((1 -> 2) + (2 -> 3) + (3 -> 4) + (4 -> 5))")
  }

  test("tree pretty"){
    assert(pretty(tree(test_tree), Seq()) == "((root -> (A + B + C)) + (A -> (AA + AB + AC)) + (B -> (BA + BB)) + (BA -> BAA))")
  }

  test("tree toAdj"){
    assert(toAdj(tree(test_tree)) == Map("AC" -> Set(), "root" -> Set("A", "B", "C"), "AA" -> Set(), "BAA" -> Set(), "C" -> Set(), "AB" -> Set(), "A" -> Set("AA", "AB", "AC"), "BB" -> Set(), "BA" -> Set("BAA"), "B" -> Set("BA", "BB")))
  }
