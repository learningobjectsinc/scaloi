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

import scalaz.syntax.std.option._
import scalaz.{MonadPlus, Monoid}

/** Operations on [[PartialFunction]]s.
  */
final class PartialFunctionOps[A, R](private val self: A =∂> R) extends AnyVal {
  /** Apply this partial function to `a`, or return `default` if not defined.
    *
    * This method exists because [[PartialFunction.applyOrElse]] has a truly
    * evil signature replete with variance, bounded type parameters, and other
    * inference-confounding misfeatures.
    *
    * @param a       the value to apply this partial function to
    * @param default the default value
    * @return the result, or the default
    */
  def applyOrDefault(a: A, default: => R): R =
    self.applyOrElse[A, R](a, _ => default)

  /** Apply this partial function to `a`, or return the monoidal zero.
    *
    * @param a the value
    * @param R the monoid
    * @return the result, or monoidal zero.
    */
  def applyOrZero(a: A)(implicit R: Monoid[R]): R =
    applyOrDefault(a, R.zero)

  /** Apply this partial function inside of a [[MonadPlus]].
    *
    * @param a  the value
    * @param F  the monadic plusity
    * @tparam F the monad plus
    * @return the result, or monadic plush emptiness.
    */
  def mapply[F[_]](a: F[A])(implicit F: MonadPlus[F]): F[R] =
    F.bind(a)(self.lift.apply(_).cata(F.point(_), F.empty))
}

trait ToPartialFunctionOps {
  import language.implicitConversions

  @inline
  implicit final def ToPartialFunctionOps[A, R](pf: A =∂> R): PartialFunctionOps[A, R] =
    new PartialFunctionOps[A, R](pf)
}
