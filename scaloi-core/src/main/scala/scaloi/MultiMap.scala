package scaloi

import scalaz.Monoid

/**
  * Extension methods for dealing with multi-valued maps.
  */
object MultiMap {
  type MultiMap[K, V] = Map[K, Set[V]]


  /**
    * TODO Generalize Seq to M: ApplicativePlus?
    */
  implicit class MultiMapOps[K,V](private val map: MultiMap[K, V]) extends AnyVal {

    def add(k: K, v: V): MultiMap[K,V] =
      map + (k -> (map.getOrElse(k, Set.empty) + v))

    @inline
    def add(kv: (K,V)): MultiMap[K,V] =
      add(kv._1,kv._2)

    def add(k: K, v: Set[V]): MultiMap[K,V] =
      map + (k -> (map.getOrElse(k, Set.empty) | v))

    def append(right: MultiMap[K, V]): MultiMap[K, V] =
      right.foldLeft(map) {
        (res, kvs) => kvs match {
          case (k, vs) => res.add(k, vs)
        }
      }

    def invert: MultiMap[V, K] =
      map.toSeq
        .flatMap { case (k, vs) => vs map (_ -> k) }
        .groupBy[V](_._1)
        .map { case (v, vks) => v -> vks.map(_._2).toSet }

    // can't call this compose or it gets shadowed by PartialFunction's compose */
    def chain[W](next: MultiMap[V, W]): MultiMap[K, W] =
      map.map { case (k, i) => k -> i.flatMap(next.get).flatten }
  }

  implicit def MultiMapMonoid[K, V]: Monoid[MultiMap[K, V]] =
    new Monoid[MultiMap[K, V]] {
      def zero: MultiMap[K, V] = Map.empty

      def append(f1: MultiMap[K, V], f2: => MultiMap[K, V]) = f1 append f2
    }

}
