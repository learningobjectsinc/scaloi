package scaloi
package syntax

import scala.collection.GenTraversable

final class CollectionOps[T](val self: GenTraversable[T]) extends AnyVal {

  /** Calculate the cross product of `self` and `other`.
    *
    * The cross product of two collections is a collection containing
    * all possible pairs of elements from each collection.
    *
    * @param other the other collection to cross with `self`
    * @return a (lazy) iterable of all possible pairs of elements from each collection
    */
  def cross[U](other: GenTraversable[U]): Iterable[(T, U)] = for {
    t <- self.toStream
    u <- other.toStream
  } yield (t, u)

  /** An alias for `cross`. */
  @inline def ×[U](other: GenTraversable[U]): Iterable[(T, U)] = cross(other)

  /** An alias for `cross`. */
  @inline def ⟗ [U](other: GenTraversable[U]): Iterable[(T, U)] = cross(other)
}

trait ToCollectionOps {
  import language.implicitConversions

  @inline implicit final def toCollectionOps[T](self: GenTraversable[T]): CollectionOps[T] =
    new CollectionOps[T](self)
}

object CollectionOps extends ToCollectionOps
