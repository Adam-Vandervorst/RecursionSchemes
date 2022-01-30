package examples.file_system

import compiletime.summonInline

import be.adamv.recursionschemes.{*, given}

enum System[X]:
  case Dir(name: String, contents: Seq[X])
  case File(name: String, format: String, content: String)
  case Link(get: Fix[System] => System[X])
import System._

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

def retrieve(names: List[String], e: String)(using fs: Fix[System]) = ???

//  nav[System, Option, Fix[System]] {
//  [A] => (s: System[A]) => s match
//    case Dir(n, cs) =>
//    case File(n, s, c) => File(n, s, c)
//    case Link(get) => Link(fs => get(fs).map(f))
//}{
//  case Dir(n, cs) =>
//  case File(n, s, c) => File(n, s, c)
//  case Link(get) => Link(fs => get(fs).map(f))
//}(fs)

def retrieve_path(path: String)(root: Fix[System]) =
  val i = path.lastIndexOf(".")
  val (name_path, extension) = if i == -1 then (path, "") else path.splitAt(i)
  retrieve(name_path.split("/").toList, extension.drop(1))(using root)

def files(using fs: Fix[System]) = cata[System, Seq[Fix[System]]]{
  case Dir(_, contents) => contents.flatten
  case Link(get) => get(fs).sequence.map(Fix(_))
  case f => f.sequence.map(Fix(_))
}

def search(sub: String)(using fs: Fix[System]) = weak_para[System, Seq[Fix[System]]]{
  case (Dir(n, cs), d) if n.contains(sub) => d +: cs.flatten
  case (Dir(_, cs), _) => cs.flatten
  case (File(n, _, _), f) if n.contains(sub) => Seq(f)
  case (Link(get), _) => ???
  case _ => Seq()
}
