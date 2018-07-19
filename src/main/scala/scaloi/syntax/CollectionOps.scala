package scaloi
package syntax

import scalaz.{Liskov, Monoid, \/}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.ListSet
import scala.collection.{GenTraversableOnce, mutable}

final class CollectionOps[CC[X] <: GenTraversableOnce[X], T](val self: CC[T]) extends AnyVal {
  import Liskov._

  /** Calculate the cross product of `self` and `other`.
    *
    * The cross product of two collections is a collection containing
    * all possible pairs of elements from each collection.
    *
    * @param other the other collection to cross with `self`
    * @return a (lazy) iterable of all possible pairs of elements from each collection
    */
  def cross[U](other: GenTraversableOnce[U]): Iterable[(T, U)] = for {
    t <- self.toStream
    u <- other.toStream
  } yield (t, u)

  /** An alias for `cross`. */
  @inline def ×[U](other: GenTraversableOnce[U]): Iterable[(T, U)] = cross(other)

  /** An alias for `cross`. */
  @inline def ⟗ [U](other: GenTraversableOnce[U]): Iterable[(T, U)] = cross(other)

  @inline def squared: Iterable[(T, T)] = this ⟗ self

  def makeSerializable(implicit SF: SerializableForm[CC]): CC[T] with Serializable =
    self match {
      case s: (CC[T] @unchecked) with Serializable => s
      case _                                       => SF.makeSerializable(self)
    }

  /**
    * Group a seq to a map of values grouped by the specified value function.
    *
    * @param keyFn    The function transforming the entries to map keys.
    * @param valueFn  The function transforming the entries to values in the map.
    * @tparam K       The key type.
    * @tparam V       The grouped value type.
    * @return         Map of values grouped by the given key function
    */
  def groupMap[K, V, That](keyFn: T => K)(valueFn: T => V)(
    implicit cbf: CanBuildFrom[CC[V], V, That],
  ): Map[K, That] = {
    val result = mutable.Map
      .empty[K, mutable.Builder[V, That]]
      .withDefault(_ => cbf())
    self.foreach { t =>
      val k = keyFn(t)
      result(k) = result(k) += valueFn(t)
    }
    result.mapValues(_.result()).toMap
  }

  /** Group this collection of pairs into a multimap.
    *
    * Similar to [[scala.collection.TraversableLike.toMap toMap]], but keys are
    * aggregated into the same kind of collection as this one.
    */
  def groupToMap[K, V, That](
    implicit
    kv: T <~< (K, V),
    cbf: CanBuildFrom[CC[V], V, That],
  ): Map[K, That] =
    groupMap(t => kv(t)._1)(t => kv(t)._2)

  /** Group the elements of this collection by `kf`, map them by `vf`, and fold
    * them as elements of the monoid `V`.
    */
  def groupMapFold[K, V](kf: T => K)(vf: T => V)(implicit V: Monoid[V]): Map[K, V] = {
    val result = mutable.Map.empty[K, V].withDefaultValue(V.zero)
    self.foreach { t =>
      val k = kf(t)
      result(k) = V.append(result(k), vf(t))
    }
    result.toMap
  }

  /** Collect elements of this collection into one of two result collections,
    * possibly of different types.
    */
  def partitionCollect[A, B, CCA, CCB](f: PartialFunction[T, A \/ B])(
    implicit
    cbfA: CanBuildFrom[CC[T], A, CCA],
    cbfB: CanBuildFrom[CC[T], B, CCB],
  ): (CCA, CCB) = {
    val (as, bs) = (cbfA(), cbfB())
    self.foreach(f.runWith(_.fold(as.+=, bs.+=)))
    (as.result, bs.result)
  }

  /**
    * Apply a map to an optional value to the elements of this traversable, returning
    * the first defined result.
    *
    * @param f the map function
    * @tparam B the target type
    * @return the optional value
    */
  @inline final def findMap[B](f: T => Option[B]): Option[B] = {
    val i = self.toIterator
    @tailrec def loop: Option[B] =
      if (i.hasNext) {
        val ob = f(i.next())
        if (ob.isDefined) {
          ob
        } else {
          loop
        }
      } else {
        None
      }
    loop
  }

}

trait ToCollectionOps {
  import language.implicitConversions

  @inline implicit final def toCollectionOps[CC[X] <: GenTraversableOnce[X], T](self: CC[T]): CollectionOps[CC, T] =
    new CollectionOps[CC, T](self)
}

object CollectionOps extends ToCollectionOps


trait SerializableForm[CC[X] <: GenTraversableOnce[X]] {
  def makeSerializable[T](it: CC[T]): CC[T] with Serializable
}

//noinspection TypeAnnotation
object SerializableForm {
  implicit val setSerializableForm =
    new SerializableForm[Set] {
      def makeSerializable[T](it: Set[T]) = it.to[ListSet]
    }

  implicit val seqSerializableForm =
    new SerializableForm[Seq] {
      def makeSerializable[T](it: Seq[T]) = it.toVector
    }
}
