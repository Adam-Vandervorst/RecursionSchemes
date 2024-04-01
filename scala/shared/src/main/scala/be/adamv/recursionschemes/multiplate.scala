package be.adamv.recursionschemes

type Projector[P[_[_]], A] = [F[_]] =>> P[F] => (A => F[A])

trait MultiPlate[P[_[_]]]:
  def multiplate[F[_] : Applicative](child: => P[F]): P[F]
  def mkPlate[F[_]](build: [A] => Projector[P, A][F] => (A => F[A])): P[F]

def purePlate[P[_[_]] : MultiPlate, F[_] : Applicative]: P[F] =
  summon[MultiPlate[P]].mkPlate[F]([A] => (_: Projector[P, A][F]) => summon[Applicative[F]].pure)

def appendPlate[P[_[_]] : MultiPlate, O : Monoid](f1: P[[X] =>> O])(f2: P[[X] =>> O]): P[[X] =>> O] =
  summon[MultiPlate[P]].mkPlate[[X] =>> O]([A] => (proj: Projector[P, A][[X] =>> O]) => a => proj(f1)(a) add proj(f2)(a))

def kleisliComposePlate[P[_[_]] : MultiPlate, M[_] : Monad](f1: P[M])(f2: P[M]): P[M] =
  summon[MultiPlate[P]].mkPlate[M]([A] => (proj: Projector[P, A][M]) => a => proj(f1)(a).flatMap(b => proj(f2)(b)))

def preorderFold[P[_[_]] : MultiPlate, O : Monoid](f: P[[X] =>> O]): P[[X] =>> O] =
  appendPlate(f)(summon[MultiPlate[P]].multiplate[[X] =>> O](preorderFold[P, O](f)))

def mapFamilyM[P[_[_]] : MultiPlate, M[_] : Monad, O : Monoid](f: P[M]): P[M] =
  kleisliComposePlate(f)(summon[MultiPlate[P]].multiplate(mapFamilyM(f)))

def traverseFor[P[_[_]] : MultiPlate, A](proj: Projector[P, A][Box])(f: P[Box]): A => A =
  a => proj(f)(a).unBox
