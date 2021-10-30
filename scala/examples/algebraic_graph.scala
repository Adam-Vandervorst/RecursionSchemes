enum Graph[+A]:
  case Empty extends Graph[Nothing]
  case Vertex(a: Double)
  case Overlay(x: Graph[A], y: Graph[A])
  case Connect(x: Graph[A], y: Graph[A])

given Functor[Graph] with
  extension [A](g: Graph[A])
    def map[B](f: A => B): Graph[B] = g match
      case Empty => Empty
      case Vertex(a) => Vertex(a)
      case Overlay(x, y) => Overlay(f(x), f(y))
      case Connect(x, y) => Connect(f(x), f(y))

def flow = cata[Graph, (Seq[(Double, Double)], Double)]{
  case Empty => (Seq((0, 0)), 0)
  case Vertex(a) => (Seq((a, 1)), 0)
  case Overlay((xp, xf), (yp, yf)) => (xp ++ yp, xf + yf)
  case Connect((xp, xf), (yp, yf)) =>
    val ((fx, cx), (fy, cy)) = (xp.sum, yp.sum)
    val r = (fx + fy, cx + cy)
    val p = r._1/r._2
    (Seq(r), xf + yf + fx*(fx/cx - p) + fy*(fy/cy - p))
}
