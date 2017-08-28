package scaloi

import scala.collection.GenTraversableOnce
import scalaz.Monoid

/**
  * Extension methods for dealing with multi-valued maps.
  */
object MultiMap {
  /** A "multimap" from `K` to `V`. Just a type alias. */
  type MultiMap[K, V] = Map[K, Seq[V]]

  /** Make an empty multimap. */
  @inline
  def empty[K, V]: MultiMap[K, V] = Map.empty

  /** Operations on multimaps. */
  /* TODO Generalize Seq to M: ApplicativePlus? */
  implicit class MultiMapOps[K,V](private val map: MultiMap[K, V]) extends AnyVal {

    /** Add a new `(key, value)` pair to this multimap. */
    def add(k: K, v: V): MultiMap[K,V] =
      map + (k -> (map.getOrElse(k, Vector.empty) :+ v))

    /** Add new `(key, value)` pairs to this multimap. */
    def add(kvs: (K, V)*): MultiMap[K,V] =
      kvs.foldLeft(map) { (m, kv) => m.add(kv._1, kv._2) }

    /** Add a set of values to this multimap, associated with a given key. */
    def add(k: K, v: GenTraversableOnce[V]): MultiMap[K,V] =
      map + (k -> (map.getOrElse(k, Vector.empty) ++ v))

    /** Add all of the key-value pairs in `right` to this multimap. */
    def combine(right: MultiMap[K, V]): MultiMap[K, V] =
      right.foldLeft(map) {
        (res, kvs) => kvs match {
          case (k, vs) => res.add(k, vs)
        }
      }

    /** Create a multimap in which each value is paired with all keys
      * which map to it.
      */
    def invert: MultiMap[V, K] =
      map.toSeq
        .flatMap { case (k, vs) => vs map (_ -> k) }
        .groupBy[V](_._1)
        .map { case (v, vks) => v -> vks.map(_._2) }

    /** Create a new multimap in which the key-value mappings are
      * exactly those pairs `(k, w)` for which there exists a `v`
      * such that `(k, v)` is in this multimap, and `(v, w)` is in
      * `right`.
      */
    // can't call this compose or it gets shadowed by PartialFunction's compose */
    def chain[W](next: MultiMap[V, W]): MultiMap[K, W] =
      map
        .map { case (k, i) => k -> i.flatMap(next.get).flatten }
        .filter { case (_, w) => w.nonEmpty }

  }

  /** For a fixed `K` and `V`, `MultiMap[K, V]` is a monoid. */
  implicit def MultiMapMonoid[K, V]: Monoid[MultiMap[K, V]] =
    new Monoid[MultiMap[K, V]] {
      def zero: MultiMap[K, V] = Map.empty

      def append(f1: MultiMap[K, V], f2: => MultiMap[K, V]) = f1 combine f2
    }

}
