package lib

def cata[F[_] : Functor, A](alg: F[A] => A)(fix: Fix[F]): A =
  alg(fix.unFix.map(cata(alg)))

def para[F[_] : Functor, A](alg: F[(A, Fix[F])] => A)(fix: Fix[F]): A =
  alg(fix.unFix.map(ff => (para(alg)(ff), ff)))

def weak_para[F[_] : Functor, A](alg: (F[A], Fix[F]) => A)(fix: Fix[F]): A =
  alg(fix.unFix.map(weak_para(alg)), fix)

def bicata[F[_] : Functor, A, B](f: (F[A], F[B]) => A, g: (F[A], F[B]) => B)(fix: Fix[F]): (A, B) =
  val r = fix.unFix.map(bicata(f, g))
  val fa = r.map(_._1)
  val fb = r.map(_._2)
  (f(fa, fb), g(fa, fb))

def pre_zygo[F[_] : Functor, A, B](helper: F[B] => B, alg: (F[(A, B)], B) => A)(fix: Fix[F]): (A, B) =
  val r = fix.unFix.map(pre_zygo(helper, alg))
  val b = helper(r.map(_._2))
  (alg(r, b), b)

def ana[F[_] : Functor, A](coalg: A => F[A])(seed: A): Fix[F] =
  Fix(coalg(seed).map(ana(coalg)))

def prothesi[F[_] : Functor, A](alg: (F[A], Seq[Fix[F]]) => A)(fix: Fix[F], todo: Seq[Fix[F]] = Seq()): A =
  alg(fix.unFix.map(ff => prothesi(alg)(ff, fix +: todo)), todo)

def ichno[F[_] : Functor, A](coalg: (A, Seq[F[A]]) => F[A])(seed: A, trace: Seq[F[A]] = Seq()): Fix[F] =
  val fa = coalg(seed, trace)
  Fix(fa.map(ff => ichno(coalg)(ff, fa +: trace)))

def histo_[F[_]: Functor, A](alg: F[CoFree[F, A]] => A)(fix: Fix[F]): A =
  cata[F, CoFree[F, A]](x => CoFree(alg(x), x))(fix).a

def histo[F[_] : Functor, A](alg: F[CoFree[F, A]] => A)(fix: Fix[F]): A =
  def bundling(fix: Fix[F]): CoFree[F, A] =
    CoFree(histo(alg)(fix), fix.unFix.map(bundling))
  alg(fix.unFix.map(bundling))

def futu[F[_] : Functor, A](coalg: A => F[Free[F, A]])(a: A): Fix[F] =
  def picking(ff: Free[F, A]): Fix[F] = ff match
    case Free.Pure(a) => futu(coalg)(a)
    case Free.Bind(ff) => Fix(ff.map(picking))
  Fix(coalg(a).map(picking))
