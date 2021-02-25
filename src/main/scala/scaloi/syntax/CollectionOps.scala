/*
 * Copyright 2007 Learning Objects
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

import scalaz.{Liskov, Semigroup, \/}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.ListSet
import scala.collection.{GenTraversableOnce, immutable, mutable}
import scaloi.Zero

final class CollectionOps[CC[X] <: GenTraversableOnce[X], T](private val self: CC[T]) extends AnyVal {
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
  def groupMapFold[K, V](kf: T => K)(vf: T => V)(implicit V: Semigroup[V]): Map[K, V] = {
    val result = mutable.Map.empty[K, V]
    self.foreach { t =>
      val k = kf(t)
      result(k) = result.get(k).fold(vf(t))(V.append(_, vf(t)))
    }
    result.toMap
  }

  /**
    * Group the elements of this collection by `kf`, taking the first element
    * on a `kf` collision.
    *
    * Sugar for self.groupBy(kf).mapValues(_.head)
    */
  def groupUniqBy[K](kf: T => K): Map[K, T] =
    groupMapFold(kf)(identity)(Semigroup.firstSemigroup)

  /**
    * Group the elements of this collection to `vf`, taking the first element
    * on a key collision.
    */
  def groupUniqTo[V](vf: T => V): Map[T, V] =
    groupMapFold(identity)(vf)(Semigroup.firstSemigroup)

  /**
    * Group the elements of this collection by `kf`, taking the first element
    * on a `kf` collision, map the values with `vf`.
    *
    * Sugar for self.groupBy(kf).mapValues(_.head).mapValues(vf)
    */
  def groupMapUniq[K, V](kf: T => K)(vf: T => V): Map[K, V] =
    groupMapFold(kf)(vf)(Semigroup.firstSemigroup)

  /** Apply a partial function to this collection and combine the resulting
    * tuples into a map.
    */
  def collectToMap[B, C](pf: PartialFunction[T, (B, C)]): Map[B, C] = {
    val b = immutable.Map.newBuilder[B, C]
    self.foreach(pf.runWith(b += _))
    b.result
  }

  /** Map this collection into tuples and collect the result into a map.
    * An alias for [[foldToMap]].
    */
  def map2[B, C](f: T => (B, C)): Map[B, C] = {
    val b = immutable.Map.newBuilder[B, C]
    self.foreach(b += f(_))
    b.result
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

  /**
    * Apply a partial function to the elements of this traversable, returning
    * the first defined result.
    *
    * @param f the partial function
    * @tparam B the target type
    * @return the optional value
    */
  @inline final def findMapf[B](f: PartialFunction[T, B]): Option[B] =
    findMap(f.lift)

  /**
    * Given a function from [[T]] to a tuple of [[B]] and [[C]], fold this
    * traversable into a [[Map]].
    * @param f the map function
    * @tparam B the key type
    * @tparam C the value type
    * @return the resulting [[Map]]
    */
  @inline final def foldToMap[B, C](f: T => (B, C)): Map[B, C] = map2(f)

  /**
    * Short circuit a fold to the monoidal zero if this collection is empty.
    * @param f the fold function
    * @tparam U the result type with [[Zero]] evidence
    * @return the result type
    */
  @inline final def foldSC[U : Zero](f: CC[T] => U): U = if (self.isEmpty) Zero.zero[U] else f(self)

  /**
    * Short circuit a fold to the monoidal zero if this collection is empty.
    * An alias for foldSC.
    * @param f the fold function
    * @tparam U the result type with [[Zero]] evidence
    * @return the result type
    */
  @inline final def ??>[U : Zero](f: CC[T] => U): U = if (self.isEmpty) Zero.zero[U] else f(self)

  /**
    * Short circuit a function to the monoidal zero if this collection is empty.
    * @param f the function
    * @tparam U the result type with [[Zero]] evidence
    * @return the result type
    */
  @inline final def ??[U : Zero](f: => U): U = if (self.isEmpty) Zero.zero[U] else f
}

trait ToCollectionOps {
  import language.implicitConversions

  @inline implicit final def toCollectionOps[CC[X] <: GenTraversableOnce[X], T](self: CC[T]): CollectionOps[CC, T] =
    new CollectionOps[CC, T](self)
}

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
