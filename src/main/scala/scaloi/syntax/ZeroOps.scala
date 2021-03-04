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

import scala.annotation.nowarn
import scala.language.implicitConversions

/**
  * Enhancements on zeroes.
  *
  * @param self the zero
  * @tparam A the zero type
  */
final class ZeroOps[A](private val self: A) extends AnyVal {

  /** Test whether `self` is zero. */
  def isZero(implicit Z: Zero[A]): Boolean = Z.isZero(self)

  /** Test whether `self` is non-zero. */
  def nonZero(implicit Z: Zero[A]): Boolean = Z.nonZero(self)

  /** Zero out nulls. */
  def zNull(implicit Z: Zero[A], ev: Null <:< A): A = if (self == ev(null)) Z.zero else self

  /**
    * This or that if this is zero.
    */
  def |||(o: => A)(implicit Z: Zero[A]): A = if (Z.isZero(self)) o else self
}

/**
  * Implicit conversion for zero operations.
  */
trait ToZeroOps {

  /**
    * Implicit conversion from zero to the zero enhancements.
    * @param a the zero
    * @tparam A its type
    */
  @nowarn("cat=unused")
  implicit def toZeroOps[A : Zero](a: A): ZeroOps[A] = new ZeroOps(a)
}
