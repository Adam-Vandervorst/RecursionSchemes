package lib

trait Functor[F[_]]:
  extension [A](x: F[A])
    def map[B](f: A => B): F[B]

final case class Fix[F[_]](unFix: F[Fix[F]])

final case class CoFree[F[_], A](a: A, fc: F[CoFree[F, A]])

enum Free[F[_], A]:
  case Pure(a: A)
  case Bind(ff: F[Free[F, A]])
