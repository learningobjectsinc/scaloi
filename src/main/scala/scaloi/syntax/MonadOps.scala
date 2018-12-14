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

package scaloi.syntax

import scalaz.{Monad, Unapply}

/** Enhancements upon Monads. */
final class MonadOps[M[_], A](val self: M[A]) extends AnyVal {

  /**
    * Compute an effect from the contained value, then apply
    * that effect to `self`, discarding the computed value.
    *
    * @param f the effectful function
    * @param M evidence of the monadicity of `M`
    * @return `self` with the effects of `f(self)` applied
    */
  def flatTap[B](f: A => M[B])(implicit M: Monad[M]): M[A] =
    M.bind(self)(a => M.map(f(a))(_ => a))

}

object MonadOps extends ToMonadOps with ToFunctorOps

trait ToMonadOps extends ToMonadOps0 {
  import language.implicitConversions

  @inline
  implicit final def ToMonadOps[M[_] : Monad, A](self: M[A]): MonadOps[M, A] =
    new MonadOps[M, A](self)
}

trait ToMonadOps0 {
  import language.implicitConversions

  @inline
  implicit final def ToMonadOps0[MA](self: MA)(implicit UA: Unapply[Monad, MA]): MonadOps[UA.M, UA.A] =
    new MonadOps[UA.M, UA.A](UA(self))

}
