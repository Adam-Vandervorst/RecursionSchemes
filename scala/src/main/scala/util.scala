package lib

extension [K, V](m1: Map[K, V])
  def mergeWith(m2: Map[K, V])(dedup: (V, V) => V): Map[K, V] =
    (m1 -- m2.keySet) ++ m2.map((k, v) => k -> m1.get(k).map(dedup(v, _)).getOrElse(v))

type Tree[V] = Fix[[X] =>> (V, List[X])]
def node[V](s: V, xs: Tree[V]*): Tree[V] = Fix((s, xs.toList))
def asLeaf[V](t: Tree[V]): Tree[V] = Fix((t.unFix._1, List()))

extension (x: Object)
  def strHash = java.lang.Integer.toString(x.hashCode, 36).replace("-", "m")

def balanced(s: String): Option[String] =
  if !s.startsWith("(") then return None
  var j = 0; var d = 0
  while j < s.length do
    s(j) match
      case '(' => d += 1
      case ')' if d == 1 =>
        return Some(s.slice(0, j + 1))
      case ')' => d -= 1
      case _ => ()
    j += 1
  None