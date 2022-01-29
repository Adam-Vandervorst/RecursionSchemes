package be.adamv.recursionschemes

// Folds
def cata[F[_] : Functor, A](alg: F[A] => A)(fix: Fix[F]): A =
  alg(fix.unFix.map(cata(alg)))

def dist_cata[F[_] : Functor] =
  [X] => (fia: F[Box[X]]) => Box(fia.map(_.unBox))

def _cata_gcata[F[_] : Functor, A](alg: F[A] => A)(fix: Fix[F]): A =
  gcata[F, Box, A](dist_cata[F])(fba => alg(fba.map(_.unBox)))(fix)

def cataM[F[_] : Traversable, M[_] : Monad, A](alg: F[A] => M[A])(fix: Fix[F]): M[A] =
  fix.unFix.traverse(cataM(alg)).flatMap(alg)

def gcata[F[_] : Functor, W[_] : CoMonad, A](dw: [X] => F[W[X]] => W[F[X]])(alg: F[W[A]] => A)(fix: Fix[F]): A =
  def run(fa: Fix[F]): W[F[W[A]]] = dw(fa.unFix.map(f => run(f).map(alg).duplicate))
  alg(run(fix).extract)

def para[F[_] : Functor, A](alg: F[(A, Fix[F])] => A)(fix: Fix[F]): A =
  alg(fix.unFix.map(ff => (para(alg)(ff), ff)))

def dist_para[F[_] : Functor] =
  [X] => (f: F[(X, Fix[F])]) => (f.map(_._1), Fix(f.map(_._2)))

def _para_gcata[F[_] : Functor, A](alg: F[(A, Fix[F])] => A)(fix: Fix[F]): A =
  gcata[F, [X] =>> (X, Fix[F]), A](dist_para[F])(alg)(fix)

def weak_para[F[_] : Functor, A](alg: (F[A], Fix[F]) => A)(fix: Fix[F]): A =
  alg(fix.unFix.map(weak_para(alg)), fix)

def prothesi[F[_] : Functor, A](alg: (F[A], Seq[Fix[F]]) => A)(fix: Fix[F], todo: Seq[Fix[F]] = Seq()): A =
  alg(fix.unFix.map(ff => prothesi(alg)(ff, fix +: todo)), todo)

def histo[F[_] : Functor, A](alg: F[CoFree[F, A]] => A)(fix: Fix[F]): A =
  def bundling(fix: Fix[F]): CoFree[F, A] =
    CoFree(histo(alg)(fix), fix.unFix.map(bundling))
  alg(fix.unFix.map(bundling))

def dist_histo[F[_] : Functor]: [X] => F[CoFree[F, X]] => CoFree[F, F[X]] =
  [X] => (f: F[CoFree[F, X]]) => CoFree(f.map(_.a), f.map(fcf => dist_histo[F][X](fcf.fc)))

def _histo_gcata[F[_] : Functor, A](alg: F[CoFree[F, A]] => A)(fix: Fix[F]): A =
  gcata[F, [X] =>> CoFree[F, X], A](dist_histo[F])(alg)(fix)

def _histo_cata[F[_]: Functor, A](alg: F[CoFree[F, A]] => A)(fix: Fix[F]): A =
  cata[F, CoFree[F, A]](x => CoFree(alg(x), x))(fix).a

def bicata[F[_] : Functor, A, B](f: (F[A], F[B]) => A, g: (F[A], F[B]) => B)(fix: Fix[F]): (A, B) =
  val r = fix.unFix.map(bicata(f, g))
  val fa = r.map(_._1)
  val fb = r.map(_._2)
  (f(fa, fb), g(fa, fb))

def pre_zygo[F[_] : Functor, A, B](helper: F[B] => B, alg: (F[(A, B)], B) => A)(fix: Fix[F]): (A, B) =
  val r = fix.unFix.map(pre_zygo(helper, alg))
  val b = helper(r.map(_._2))
  (alg(r, b), b)

// Unfolds
def ana[F[_] : Functor, A](coalg: A => F[A])(seed: A): Fix[F] =
  Fix(coalg(seed).map(ana(coalg)))

def dist_ana[F[_] : Functor] =
  [X] => (fia: Box[F[X]]) => fia.unBox.map(Box(_))

def _ana_gana[F[_] : Functor, A](coalg: A => F[A])(seed: A): Fix[F] =
  gana[F, Box, A](dist_ana[F])(a => coalg(a).map(Box(_)))(seed)

def anaM[F[_] : Traversable, M[_] : Monad, A](coalg: A => M[F[A]])(seed: A): M[Fix[F]] =
  coalg(seed).flatMap(fa => fa.traverse(anaM(coalg)).map(Fix(_)))

def anaDistM[F[_] : Functor, M[_] : Monad, A](dm: [X] => F[M[X]] => M[F[X]])(coalg: A => M[F[A]])(seed: A): M[Fix[F]] =
  coalg(seed).flatMap(fa => dm(fa.map(a => anaDistM[F, M, A](dm)(coalg)(a))).map(Fix(_)))

def gana[F[_] : Functor, M[_] : Monad, A](dm: [X] => M[F[X]] => F[M[X]])(coalg: A => F[M[A]])(seed: A): Fix[F] =
  def run(mfma: M[F[M[A]]]): Fix[F] = Fix(dm(mfma).map(fmma => run(coalg.lift(fmma.flatten))))
  run(coalg(seed).pure)

def apo[F[_] : Functor, A](coalg: A => F[Either[A, Fix[F]]])(seed: A): Fix[F] =
  Fix(coalg(seed).map(_.fold(apo(coalg), identity)))

def weak_apo[F[_] : Functor, A](coalg: A => Either[F[A], Fix[F]])(seed: A): Fix[F] =
  coalg(seed).fold(fa => Fix(fa.map(weak_apo(coalg))), identity)

def ichno[F[_] : Functor, A](coalg: (A, Seq[F[A]]) => F[A])(seed: A, trace: Seq[F[A]] = Seq()): Fix[F] =
  val fa = coalg(seed, trace)
  Fix(fa.map(ff => ichno(coalg)(ff, fa +: trace)))

def futu[F[_] : Functor, A](coalg: A => F[Free[F, A]])(a: A): Fix[F] =
  def picking(ff: Free[F, A]): Fix[F] = ff match
    case Free.Pure(a) => futu(coalg)(a)
    case Free.Bind(ff) => Fix(ff.map(picking))
  Fix(coalg(a).map(picking))

def dist_futu[F[_] : Functor]: [X] => Free[F, F[X]] => F[Free[F, X]] =
  [X] => (f: Free[F, F[X]]) => f match
    case Free.Pure(fx) => fx.map(Free.Pure[F, X])
    case Free.Bind(ff) => ff.map(fff => Free.Bind(dist_futu[F][X](fff)))

def _futu_gana[F[_] : Functor, A](coalg: A => F[Free[F, A]])(a: A): Fix[F] =
  gana[F, [X] =>> Free[F, X], A](dist_futu[F])(coalg)(a)
