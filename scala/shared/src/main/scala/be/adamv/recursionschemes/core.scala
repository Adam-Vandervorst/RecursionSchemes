package be.adamv.recursionschemes


trait Monoid[A]:
  def zero: A
  extension (x: A) 
    infix def add(y: A): A

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]
  extension [A, B](f: A => B)
    def lift: F[A] => F[B] =
      fa => fa.map(f)

trait Applicative[F[_]] extends Functor[F]:
  extension [A](a: A)
    def pure: F[A]
  extension [A](x: F[A])
    def map[B](f: A => B): F[B] =
      pure(f).app(x)
  extension [A, B](fab: F[A => B])
    infix def app(fa: F[A]): F[B]
  extension [A, B](f: A => B)
    override def lift: F[A] => F[B] =
      fa => pure(f).app(fa)

trait Traversable[T[_]] extends Functor[T]:
  extension [A](ta: T[A])
    def traverse[F[_] : Applicative, B](f: A => F[B]): F[T[B]]
  extension [F[_] : Applicative, A](tfa: T[F[A]])
    def sequence: F[T[A]] =
      tfa.traverse(identity)

trait Monad[M[_]] extends Applicative[M]:
  extension [A](a: A)
    def pure: M[A]
  extension [A](x: M[M[A]])
    def flatten: M[A] = x.flatMap(identity)
  extension [A](x: M[A])
    def flatMap[B](f: A => M[B]): M[B]
    override def map[B](f: A => B) = x.flatMap(f.andThen(pure))
  extension [A, B](fab: M[A => B])
    def app(fa: M[A]): M[B] = fab.flatMap(fa.map)

trait CoMonad[W[_]] extends Functor[W]:
  extension [A](x: W[A])
    def extract: A
    def duplicate: W[W[A]] = x.extend(identity)
    def extend[B](f: W[A] => B): W[B] =
      duplicate.map(f)

given monad_app [F[_] : Monad]: Applicative[F] = summon[Monad[F]]
given trav_func [F[_] : Traversable]: Functor[F] = summon[Traversable[F]]
given app_func [F[_] : Applicative]: Functor[F] = summon[Applicative[F]]
given [F[_] : Functor, G[_] : Functor]: Functor[[X] =>> F[G[X]]] with
  extension [A](fga: F[G[A]])
    def map[B](m: A => B): F[G[B]] = summon[Functor[F]].map(fga)(_.map(m))
given const_monoid_app [O : Monoid]: Applicative[[X] =>> O] with
  extension [A](a: A) def pure: O = summon[Monoid[O]].zero
  extension [A, B](fab: O) 
    infix def app(fa: O): O = fab add fa

final case class Fix[F[_]](unFix: F[Fix[F]])

final case class CoFree[F[_], A](a: A, fc: F[CoFree[F, A]])

enum Free[F[_], A]:
  case Pure(a: A)
  case Bind(ff: F[Free[F, A]])
