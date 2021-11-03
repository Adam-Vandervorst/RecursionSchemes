package lib

extension [K, V](m1: Map[K, V])
  def mergeWith(m2: Map[K, V])(dedup: (V, V) => V): Map[K, V] =
    (m1 -- m2.keySet) ++ m2.map((k, v) => k -> m1.get(k).map(dedup(v, _)).getOrElse(v))

type Tree[V] = Fix[[X] =>> (V, List[X])]
def node[V](s: V, xs: Tree[V]*): Tree[V] = Fix((s, xs.toList))
def asLeaf[V](t: Tree[V]): Tree[V] = Fix((t.unFix._1, List()))
