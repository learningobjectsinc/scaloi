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

package scaloi.syntax

import scalaz.Functor

/**
  * Enhancements on functors.
  *
  * @param self the functor
  * @tparam F the functor type
  * @tparam A the functed type
  */
final class FunctorOps[F[_], A](private val self: F[A]) extends AnyVal {
  /** Apply a partial function to the values in this functor. Values for
    * which the partial function is undefined remain unchanged.
    * @param pf the partial function
    * @param F the functor evidence
    * @tparam A1 the result type
    * @return the partially transformed functor
    */
  @inline
  final def pfMap[A1 >: A](pf: PartialFunction[A, A1])(implicit F: Functor[F]): F[A1] =
    F.map(self)(fa => pf.applyOrElse(fa, (a: A) => a))

  /**
    * Inject `b` to the right of the `A`s in `self`.
    * @param b the other value
    * @tparam B the content type
    * @return the associated values
    */
  def <*-[B](b: B)(implicit F: Functor[F]): F[(A, B)] = F.strengthR(self, b)
}

/**
  * Implicit conversion for functor operations.
  */
trait ToFunctorOps {
  import language.implicitConversions

  /**
    * Implicit conversion from functor to the functor enhancements.
    * @param f the functor
    * @tparam F the functor type
    * @tparam A the functed type
    */
  @inline
  implicit final def toFunctorOps[F[_] : Functor, A](f: F[A]): FunctorOps[F, A] = new FunctorOps(f)
}
