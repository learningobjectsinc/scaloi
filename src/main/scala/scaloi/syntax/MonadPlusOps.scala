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

import scalaz.{Equal, MonadPlus, Unapply}
import scalaz.syntax.equal._
import scaloi.Zero

/** Enhancements upon MonadPlus. */
final class MonadPlusOps[M[_], A](val self: M[A]) extends AnyVal {

  /** Filter out zero values from `self`.
    *
    * @param M [[MonadPlus]] evidence for `M`
    * @param Z [[Zero]] evidence for `A`
    * @param E [[Equal]] evidence for `A`
    * @return `self' without the zeroes`
    */
  def filterNZ(implicit M: MonadPlus[M], Z: Zero[A], E: Equal[A]): M[A] =
    M.filter(self)(_ =/= Z.zero)

}

object MonadPlusOps extends ToMonadPlusOps with ToMonadOps

trait ToMonadPlusOps extends ToMonadPlusOps0 {
  import language.implicitConversions

  @inline
  implicit final def ToMonadPlusOps[M[_]: MonadPlus, A](self: M[A]): MonadPlusOps[M, A] =
    new MonadPlusOps[M, A](self)
}

trait ToMonadPlusOps0 {
  import language.implicitConversions

  @inline
  implicit final def ToMonadPlusOps0[MA](self: MA)(implicit UA: Unapply[MonadPlus, MA]): MonadPlusOps[UA.M, UA.A] =
    new MonadPlusOps[UA.M, UA.A](UA(self))

}
