package be.adamv.recursionschemes

object Combinators:
  def Y[T, R](f: (T => R) => (T => R)): T => R =
    (t: T) => f(Y(f))(t)

  def Y2[T1, T2, R](f: ((T1, T2) => R) => ((T1, T2) => R)): (T1, T2) => R =
    (t1: T1, t2: T2) => f(Y2(f))(t1, t2)

  def Y3[T1, T2, T3, R](f: ((T1, T2, T3) => R) => ((T1, T2, T3) => R)): (T1, T2, T3) => R =
    (t1: T1, t2: T2, t3: T3) => f(Y3(f))(t1, t2, t3)
