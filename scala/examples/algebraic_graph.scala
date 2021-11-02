package examples.algebraic_graph

import lib.*

enum Graph[+V, +A]:
  case Empty extends Graph[Nothing, Nothing]
  case Vertex(a: V)
  case Overlay(x: A, y: A)
  case Connect(x: A, y: A)
import Graph._

given [V]: Functor[[X] =>> Graph[V, X]] with
  extension [A](g: Graph[V, A])
    def map[B](f: A => B): Graph[V, B] = g match
      case Empty => Empty
      case Vertex(a) => Vertex(a)
      case Overlay(x, y) => Overlay(f(x), f(y))
      case Connect(x, y) => Connect(f(x), f(y))

def vertexSet[V] = cata[[X] =>> Graph[V, X], Set[V]]{
  case Empty => Set()
  case Vertex(a) => Set(a)
  case Overlay(l, r) => l | r
  case Connect(l, r) => l | r
}

def fromAdj[K, V] = futu[[X] =>> Graph[V, X], Iterable[(V, Set[V])]]{
  case (v, nbs)::tail => Overlay(Free.Pure(tail),
    Free.Bind(Connect(Free.Bind(Vertex(v)),
              nbs.map(k => Free.Bind(Vertex(k))).fold(Free.Bind(Empty))((x, y) => Free.Bind(Overlay(x, y))))))
  case Nil => Empty
}

def toAdj[V] = cata[[X] =>> Graph[V, X], Map[V, Set[V]]]{
  case Empty => Map()
  case Vertex(a) => Map(a -> Set())
  case Overlay(l, r) => l.mergeWith(r)(_ | _)
  case Connect(l, r) => l.mergeWith(r)(_ | _)
    .mergeWith(l.keySet.map((_, r.keySet)).toMap)(_ | _)
}

def pretty[V] = prothesi[[X] =>> Graph[V, X], String]((fa, todo) => fa match
  case Empty => "-"
  case Vertex(a) => a.toString
  case Overlay(x, y) => todo match
    case Fix(Overlay(_, _))::_ => s"$x + $y"
    case _ => s"($x + $y)"
  case Connect(x, y) => todo match
    case Fix(Connect(_, _))::_ => s"$x -> $y"
    case _ => s"($x -> $y)"
)
