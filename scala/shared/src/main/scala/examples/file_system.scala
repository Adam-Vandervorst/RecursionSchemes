package examples.file_system

import compiletime.summonInline

import be.adamv.recursionschemes.{*, given}

sealed trait System[+X]
case class Dir[X](name: String, contents: Seq[X]) extends System[X]
case class File(name: String, format: String, content: String) extends System[Nothing]:
  lazy val fullname: String = f"$name.$format"
case class Link[X](get: Fix[System] => System[X]) extends System[X]

given system_functor: Functor[System] with
  extension [A](e: System[A])
    def map[B](f: A => B): System[B] = e match
      case Dir(n, cs) => Dir(n, cs.map(f))
      case File(n, s, c) => File(n, s, c)
      case Link(get) => Link(fs => get(fs).map(f))

given (using fs: Fix[System]): Traversable[System] with
  extension [A](e: System[A])
    def map[B](f: A => B): System[B] = system_functor.map(e)(f)

    def traverse[F[_] : Applicative, B](f: A => F[B]): F[System[B]] = e match
      case Dir(n, cs) => ((cs: Seq[B]) => Dir(n, cs)).pure app cs.traverse(f)
      case File(n, s, c) => File(n, s, c).pure
      case Link(get) => get(fs).traverse(f)

def retrieve(using root: Fix[System]) = weak_para[System, List[String] => Option[Fix[System]]](Combinators.Y2(alg =>
  case (Dir(n, fs), res) =>
    case h :: Nil if n == h => Some(res)
    case h :: tail if n == h => fs.flatMap(_ (tail)).headOption
    case _ => None
  case (f: File, res) =>
    case h :: Nil if f.fullname == h => Some(res)
    case _ => None
  case (Link(get), res) => alg(get(root), res)
))(root)

def retrieve_path(using root: Fix[System]) =
  val retrieve_f = retrieve(using root)
  (path: String) => retrieve_f(path.split("/").toList)

def files(using fs: Fix[System]) = cata[System, Seq[Fix[System]]]{
  case Dir(_, contents) => contents.flatten
  case Link(get) => get(fs).sequence.map(Fix(_))
  case f => f.sequence.map(Fix(_))
}

def search(sub: String)(using fs: Fix[System]) = weak_para[System, Seq[Fix[System]]](Combinators.Y2(alg =>
  case (Dir(n, cs), d) if n.contains(sub) => d +: cs.flatten
  case (Dir(_, cs), _) => cs.flatten
  case (File(n, _, _), f) if n.contains(sub) => Seq(f)
  case (Link(get), l) => alg(get(fs), l)
  case _ => Seq()
))
