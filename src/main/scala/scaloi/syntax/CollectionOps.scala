package scaloi
package syntax

import scalaz.\/

import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.ListSet

final class CollectionOps[CC[X] <: GenTraversable[X], T](val self: CC[T]) extends AnyVal {

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

  @inline def squared: Iterable[(T, T)] = this ⟗ self

  def makeSerializable(implicit SF: SerializableForm[CC]): CC[T] with Serializable =
    self match {
      case s: (CC[T] @unchecked) with Serializable => s
      case _                                         => SF.makeSerializable(self)
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
}

trait ToCollectionOps {
  import language.implicitConversions

  @inline implicit final def toCollectionOps[CC[X] <: GenTraversable[X], T](self: CC[T]): CollectionOps[CC, T] =
    new CollectionOps[CC, T](self)
}

object CollectionOps extends ToCollectionOps


trait SerializableForm[CC[X] <: GenTraversable[X]] {
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
