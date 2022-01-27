package be.adamv.recursionschemes

given Monad[Option] with
  extension [A](a: A)
    def pure: Option[A] = Some(a)
  extension [A](ma: Option[A])
    override def map[B](f: A => B): Option[B] = ma.map(f)
    def flatMap[B](f: A => Option[B]): Option[B] = ma.flatMap(f)

given Traversable[Option] with
  extension [A](ta: Option[A])
    def map[B](f: A => B): Option[B] = ta.map(f)
    def traverse[F[_] : Applicative, B](f: A => F[B]): F[Option[B]] = ta match
      case Some(a) => Some[B].pure app f(a)
      case None => None.pure

given Traversable[Seq] with
  extension [A](ta: Seq[A])
    def map[B](f: A => B): Seq[B] = ta.map(f)
    def traverse[F[_] : Applicative, B](f: A => F[B]): F[Seq[B]] =
      ta.foldRight(Seq[B]().pure) { (x, ys) =>
        f(x).map((a: B) => (b: Seq[B]) => b.prepended(a)) app ys
      }

given CoMonad[Box] with
  extension [A](x: Box[A])
    def map[B](f: A => B): Box[B] = Box(f(x.unBox))
    def extract: A = x.unBox
    override def duplicate: Box[Box[A]] = Box(x)

given Monad[Box] with
  extension [A](a: A)
    def pure: Box[A] = Box(a)
  extension [A](x: Box[A])
    def flatMap[B](f: A => Box[B]): Box[B] = f(x.unBox)

given [R]: CoMonad[[L] =>> (L, R)] with
  extension [A](x: (A, R))
    def map[B](f: A => B): (B, R) = (f(x._1), x._2)
    def extract: A = x._1
    override def duplicate: ((A, R), R) = (x, x._2)

given [F[_] : Functor]: CoMonad[[X] =>> CoFree[F, X]] with
  extension [A](x: CoFree[F, A])
    def map[B](f: A => B): CoFree[F, B] = CoFree(f(x.a), x.fc.map(_.map(f)))
    def extract: A = x.a
    override def duplicate: CoFree[F, CoFree[F, A]] = CoFree(x, x.fc.map(_.duplicate))

given [F[_] : Functor]: Monad[[X] =>> Free[F, X]] with
  extension [A](a: A)
    def pure: Free[F, A] = Free.Pure(a)
  extension [A](x: Free[F, A])
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = x match
      case Free.Pure(a) => f(a)
      case Free.Bind(ff) => Free.Bind(ff.map(_.flatMap(f)))
