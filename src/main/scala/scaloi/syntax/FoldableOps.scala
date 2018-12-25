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

import scala.language.implicitConversions
import scalaz.{Foldable, Monoid}

/**
  * Enhancements on foldable things.
  * @param self the foldable thing
  * @tparam F the foldable type
  * @tparam A the folded type
  */
final class FoldableOps[F[_], A](private val self: F[A]) extends AnyVal {

  /**
    * Apply a map to an optional value to the elements of this foldable, returning
    * the first defined result.
    *
    * @param f the map function
    * @param ev foldable evidence
    * @tparam B the target type
    * @return the optional value
    */
  @inline final def findMap[B](f: A => Option[B])(implicit ev: Foldable[F]): Option[B] =
    ev.findMapM[scalaz.Id.Id, A, B](self)(f)

  /** Fold a foldable's worth of elements into another foldable's worth of
    * monoidal values, then combine those values monoidally.
    */
  @inline final def flatFoldMap[G[_], B](f: A => G[B])(
      implicit F: Foldable[F],
      G: Foldable[G],
      B: Monoid[B]
  ): B =
    F.foldMap(self)(a => G.fold(f(a)))

  /** Fold this to a [[Map]] after mapping each element to a tuple. Right bias for dups. */
  @inline final def foldToMap[B, C](f: A => (B, C))(implicit ev: Foldable[F]): Map[B, C] =
    ev.foldLeft(self, Map.empty[B, C])((bcs, a) => bcs + f(a))

}

/**
  * Implicit conversion for foldable operations.
  */
trait ToFoldableOps {

  /**
    * Implicit conversion from foldable to its enhancements.
    * @param f: the foldable thing
    * @tparam F the foldable type
    * @tparam A the folded type
    */
  implicit def toFoldableOps[F[_]: Foldable, A](f: F[A]): FoldableOps[F, A] =
    new FoldableOps(f)
}
