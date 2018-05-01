package scaloi
package syntax

import scalaz.Monoid

import scala.collection.immutable.ListMap

/**
  * Enhancements on immutable maps.
  *
  * @tparam K the key type
  * @tparam V the value type
  */
final class MapOps[K, V](private val self: Map[K, V]) extends AnyVal {

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

  def getOrZero(key: K)(implicit V: Monoid[V]): V =
    self.getOrElse(key, V.zero)

  /** Add mappings to `None` to this map to ensure that every key in `keys`
    * exists in the map.
    */
  def ensureKeys(keys: Set[K])(implicit ev: None.type <:< V): Map[K, V] =
    keys.map(_ -> ev(None)).toMap ++ self

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

  /** If the values of this map are of numeric type, an [[Iterable]] containing
    * the keys repeated by multiplicity given by their values.
    *
    * @note The name is meant to invoke `flatten` without clashing.
    */
  def raze(implicit ev: Numeric[V]): Iterable[K] =
    self.flatMap {
      case (k, v) => Iterator.fill(ev.toInt(v))(k)
    }
}

/** Map ops companion. */
object MapOps extends ToMapOps

/** Trait containing the implicit conversion from maps to map ops */
trait ToMapOps {
  import language.implicitConversions

  @inline
  final implicit def ToMapOps[K, V](self: Map[K, V]): MapOps[K, V] =
    new MapOps[K, V](self)

}
