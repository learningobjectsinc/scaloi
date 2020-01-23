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

import scalaz.Id.Id
import scaloi.misc.Hypermonad

import scala.language.implicitConversions

/**
  * Hypermonadic syntax.
  *
  * @param fa the functored value
  * @tparam A the wrapped type
  */
final class HypermonadOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def flatterMap[G[_], H[_], B](f: A => G[H[B]])(implicit hyper: Hypermonad[F, G, H]): F[B] =
    hyper.flatterMap(fa, f)

  def hyperFlatMap[G[_], H[_], B](f: A => G[H[B]])(implicit hyper: Hypermonad[F, G, H]): F[B] =
    hyper.flatterMap(fa, f)

  def hyperFlatMap1[G[_], B](f: A => G[B])(implicit hyper: Hypermonad[F, G, Id]): F[B] =
    hyper.flatterMap(fa, f)

  def hyperFlatten[G[_], H[_], B](implicit ev: A <:< G[H[B]], hyper: Hypermonad[F, G, H]): F[B] =
    hyper.flatterMap(fa, ev)

  def hyperFlatten1[G[_], B](implicit ev: A <:< G[B], hyper: Hypermonad[F, G, Id]): F[B] =
    hyper.flatterMap(fa, ev)
}

/**
  * Hypermonadic syntax when you know you're hyper.
  *
  * @param fgha the hypermonad value
  * @tparam A the wrapped type
  */
final class EndoHypermonadOps[F[_], G[_], H[_], A](private val fgha: F[G[H[A]]]) extends AnyVal {
  def hyperFlattenE(implicit hyper: Hypermonad[F, G, H]): F[A] =
    hyper.flatterMap(fgha, (gha: G[H[A]]) => gha)
}

/**
  * Implicit conversion for hypermonadic operations.
  */
trait ToHypermonadOps {

  /** Implicit conversion to hypermonadic syntax. */
  implicit def toHypermonadOps[F[_], A](fa: F[A]): HypermonadOps[F, A] = new HypermonadOps(fa)

  /** Implicit conversion to endohypermonadic syntax. */
  implicit def toEndoHypermonadOps[F[_], G[_], H[_], A](fgha: F[G[H[A]]]): EndoHypermonadOps[F, G, H, A] =
    new EndoHypermonadOps[F, G, H, A](fgha)
}
