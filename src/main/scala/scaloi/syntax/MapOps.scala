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
package syntax

import scalaz._

import scala.collection.immutable.ListMap
import scalaz.syntax.std.option._

/**
  * Enhancements on immutable maps.
  *
  * @tparam K the key type
  * @tparam V the value type
  */
final class MapOps[K, V](private val self: Map[K, V]) extends AnyVal {

  /**
    * Return an immutable map with a default value of the monoidal zero.
    * @param ev monoid evidence for the value type
    * @return the mutable map
    */
  @inline final def withDefaultZero(implicit ev: Monoid[V]): Map[K, V] = self.withDefaultValue(ev.zero)

  /**
    * Create a new map containing only those keys for
    * which `f` returns `true`.
    *
    * @note that this has the same behavior as the standard
    *       method [[scala.collection.immutable.Map.filterKeys]],
    *       but is eager (it evaluates `f` once per key-value
    *       pair on call, where `filterKeys` evaluates `f` every
    *       time the map is queried).
    *
    * @param f the filtering predicate
    */
  def filterKeysEagerly(f: K => Boolean): Map[K, V] =
    self.filter({ case (k, _) => f(k) })

  /**
    * Create a new map containing only those values
    * for which `f` returns `true`.
    *
    * @param f the filtering predicate
    */
  def filterValues(f: V => Boolean): Map[K, V] =
    self.filter({ case (_, v) => f(v) })

  /**
    * Create a new map in which the keys of this map are
    * mapped to the result of applying `f` to the values
    * of this map.
    *
    * @note that this has the same behavior as the standard
    *       method [[scala.collection.immutable.Map.mapValues]],
    *       but is eager (it evaluates `f` once per key-value
    *       pair on call, where `mapValues` evaluates `f` every
    *       time the map is queried).
    *
    * @param f the mapping function
    */
  def mapValuesEagerly[W](f: V => W): Map[K, W] =
    self.map({ case (k, v) => k -> f(v) })

  def makeSerializable: Map[K, V] with Serializable = self match {
    case s: Serializable => s
    case _               => (ListMap.newBuilder[K, V] ++= self).result()
  }

  /** Get the mapped value or the monoidal zero. */
  def getOrZero(key: K)(implicit V: Monoid[V]): V =
    self.getOrElse(key, V.zero)

  /**
    * Get to a disjunction.
    * @param key the key
    * @return either the mapped value on the right or the key on the left
    */
  def getRightDisjunction(key: K): K \/ V = self.get(key).toRightDisjunction(key)

  /**
    * Get to a disjunction.
    * @param key the key
    * @param left the left value to return if missing
    * @return either the mapped value on the right or the supplied value on the left
    */
  def getRightDisjunction[A](key: K, left: => A): A \/ V = self.get(key).toRightDisjunction(left)

  /** Modify the value at `key` with the provided function.
    *
    * Removes the key from the map if `f` returns `None`.
    *
    * @param key the key at which to update
    * @param f   the function with which to update
    * @return the map, updated thus
    * @see [[scalaz.==>>.update the scalaz analogue]]
    */
  def update(key: K)(f: V => Option[V]): Map[K, V] =
    self.get(key).flatMap(f) match {
      case Some(newV) => self + (key -> newV)
      case None       => self - key
    }

  /** Modify the value at `key` with the provided function.
    *
    * @param key the key at which to update
    * @param f   the function with which to update
    * @return the map, updated thus
    * @see [[scalaz.==>>.update the scalaz analogue]]
    */
  def adjust(key: K)(f: V => V): Map[K, V] =
    self.get(key) match {
      case Some(v) => self + (key -> f(v))
      case None    => self
    }

  /** If the values of this map are of numeric type, an [[Iterable]] containing
    * the keys repeated by multiplicity given by their values.
    *
    * @note The name is meant to invoke `flatten` without clashing.
    */
  def raze(implicit ev: Numeric[V]): Iterable[K] =
    self.flatMap {
      case (k, v) => Iterator.fill(ev.toInt(v))(k)
    }

  /** Combine these two maps into a single map with keys drawn from either map,
    * using the [[Semigroup]] instance for `V` to combine multiple values for a
    * single key.
    */
  def combine(other: Map[K, V])(implicit V: Semigroup[V]): Map[K, V] =
    combineWith(other)(V.append(_, _))

  /** Combine these two maps into a single map with keys drawn from either map,
    * using the provided function to combine multiple values for a single key.
    */
  def combineWith(other: Map[K, V])(f: (V, V) => V): Map[K, V] = {
    import scaloi.syntax.option._
    (self.keySet | other.keySet).iterator.map { k =>
      k -> (self.get(k) \&/ other.get(k)).get.fold(identity, identity, f)
    }.toMap
  }

  /** Traverse the keys of the map into an applicative. */
  def traverseKeys[G[_]: Applicative,B](f: K => G[B]): G[Map[B,V]] = {
    self.foldLeft(Applicative[G].point(Map.empty[B,V]))({ case (gb,(k,v)) =>
        Applicative[G].apply2(gb,f(k))((bv,k) => bv.updated(k,v))
    })
  }

  import scaloi.syntax.set._

  /** Remap the keys in this map to new values, discarding current values.
    *
    * @param f mapping function for new values
    * @tparam U resulting value type
    * @return the new map
    */
  def remap[U](f: K => U): Map[K, U] = self.keySet.mapTo(f)
}

/** Trait containing the implicit conversion from maps to map ops */
trait ToMapOps {
  import language.implicitConversions

  @inline
  final implicit def ToMapOps[K, V](self: Map[K, V]): MapOps[K, V] =
    new MapOps[K, V](self)

}
