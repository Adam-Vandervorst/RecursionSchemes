package lib

trait Functor[F[_]]:
  extension [A](x: F[A])
    def map[B](f: A => B): F[B]

final case class Fix[F[_]](unFix: F[Fix[F]])

final case class CoFree[F[_], A](a: A, fc: F[CoFree[F, A]])

enum Free[F[_], A]:
  case Pure(a: A)
  case Bind(ff: F[Free[F, A]])

extension [K, V](m1: Map[K, V])
  def mergeWith(m2: Map[K, V])(dedup: (V, V) => V): Map[K, V] =
    (m1 -- m2.keySet) ++ m2.map((k, v) => k -> m1.get(k).map(dedup(v, _)).getOrElse(v))
