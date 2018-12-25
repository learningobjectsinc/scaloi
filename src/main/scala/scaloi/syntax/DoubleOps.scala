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

import java.{lang => jl}

/** Enhancements on [[Double]]s. */
final class DoubleOps(private val self: Double) extends AnyVal {
  import double._
  import jl.Double._

  /** Check whether this [[Double]] is within [[ε]] of `other`. */
  def ≈(other: Double): Boolean =
    longBitsToDouble(doubleToRawLongBits(self - other) & SignMasque) < ε

  /** Check whether this [[Double]] is less than `other`, within [[ε]]. */
  def ⪅(other: Double): Boolean = self < other || (this ≈ other)

  /** Check whether this [[Double]] is greater than `other`, within [[ε]]. */
  def ⪆(other: Double): Boolean = self > other || (this ≈ other)

}

trait DoubleVals {
  final val ε                          = 0.0001d // capriciously chosen
  private[syntax] final val SignMasque = 0x7FFFFFFFFFFFFFFFL
}

trait ToDoubleOps {
  import language.implicitConversions

  @inline
  implicit final def ToDoubleOps(self: Double): DoubleOps = new DoubleOps(self)

}
