package scaloi

/**
  * Extension methods for dealing with multi valued maps.
  */
object MultiMap {
  type MultiMap[K,V] = Map[K,Set[V]]

  /**
    * TODO Generalize Seq[V] to M[V] : Monoid
    */
  implicit class MultiMapOps[K,V](val map: MultiMap[K,V]) extends AnyVal {
    def add(k: K, v: V): MultiMap[K,V] = map + (k -> (map.getOrElse(k,Set.empty) + v))
    def add(kv: (K,V)): MultiMap[K,V] = add(kv._1,kv._2)
    def add(k: K, v: Set[V]): MultiMap[K,V] = map + (k -> (map.getOrElse(k,Set.empty) | v))
  }
}
