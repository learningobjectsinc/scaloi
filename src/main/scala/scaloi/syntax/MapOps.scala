package scaloi
package syntax

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
