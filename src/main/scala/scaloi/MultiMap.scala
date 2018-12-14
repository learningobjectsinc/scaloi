/*
 * Copyright 2007 Cengage Learning, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scaloi

import scala.collection.GenTraversableOnce
import syntax.HypermonadOps._

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
  implicit class MultiMapOps[K, V](private val map: MultiMap[K, V]) extends AnyVal {

    /** Add a new `(key, value)` pair to this multimap. */
    def add(k: K, v: V): MultiMap[K, V] =
      map + (k -> (map.getOrElse(k, Vector.empty) :+ v))

    /** Add new `(key, value)` pairs to this multimap. */
    def add(kvs: (K, V)*): MultiMap[K, V] =
      kvs.foldLeft(map) { (m, kv) =>
        m.add(kv._1, kv._2)
      }

    /** Add a set of values to this multimap, associated with a given key. */
    def add(k: K, v: GenTraversableOnce[V]): MultiMap[K, V] =
      map + (k -> (map.getOrElse(k, Vector.empty) ++ v))

    /** Add all of the key-value pairs in `right` to this multimap. */
    def combine(right: MultiMap[K, V]): MultiMap[K, V] =
      right.foldLeft(map) { (res, kvs) =>
        kvs match {
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
        .map { case (k, i) => k -> i.flatterMap(next.get) }
        .filter { case (_, w) => w.nonEmpty }

    /** All key-value pairs in this multimap.
      */
    def distributed(): Iterator[(K, V)] =
      map.iterator.flatMap { case (k, vs) => vs.iterator map (k -> _) }
  }
}
