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
package misc

import java.{lang => jl}

/**
  * Typeclass evidence of an isomorphism between a primitive number type and its boxing.
  *
  * Boxes[From, To] is defined only if To is the object type boxing From.
  *
  * @tparam From the unboxed type
  * @tparam To the boxed type
  */
sealed trait Boxes[From <: AnyVal, To] {
  def box(f: From): To
  def unbox(f: To): From
}

object Boxes {
  private case class Impl[From <: AnyVal, To >: Null](
    b: From => To, u: To => From
  ) extends Boxes[From, To] {
    def box(f: From) = b(f)
    def unbox(t: To) = u(t)
  }

  implicit val boxesByte: Boxes[Byte, jl.Byte] =
    Impl[Byte, jl.Byte](jl.Byte.valueOf, _.byteValue)
  implicit val boxesShort: Boxes[Short, jl.Short] =
    Impl[Short, jl.Short](jl.Short.valueOf, _.shortValue)
  implicit val boxesInt: Boxes[Int, jl.Integer] =
    Impl[Int, jl.Integer](jl.Integer.valueOf, _.intValue)
  implicit val boxesLong: Boxes[Long, jl.Long] =
    Impl[Long, jl.Long](jl.Long.valueOf, _.longValue)
  implicit val boxesFloat: Boxes[Float, jl.Float] =
    Impl[Float, jl.Float](jl.Float.valueOf, _.floatValue)
  implicit val boxesDouble: Boxes[Double, jl.Double] =
    Impl[Double, jl.Double](jl.Double.valueOf, _.doubleValue)
  implicit val boxesBoolean: Boxes[Boolean, jl.Boolean] =
    Impl[Boolean, jl.Boolean](jl.Boolean.valueOf, _.booleanValue)
}
