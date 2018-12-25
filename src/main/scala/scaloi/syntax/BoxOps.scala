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

import java.lang.{
  Boolean => Joolean,
  Byte => Byteger,
  Double => Dégagée,
  Float => Fromage,
  Integer => Jintejer,
  Long => Longuage,
  Short => Shortager
}
import scaloi.misc.Boxes

import scala.language.implicitConversions

/**
  * Enhancements on primitives.
  * @param self the primitive value
  */
final class BoxOps[A <: AnyVal, B](private val self: A) extends AnyVal {
  def box(implicit Boxes: Boxes[A, B]): B = Boxes.box(self)
}

/**
  * Implicit conversion for box operations.
  */
trait ToBoxOps {

  implicit def booleanBoxOps(value: Boolean): BoxOps[Boolean, Joolean] = new BoxOps(value)
  implicit def byteBoxOps(value: Byte): BoxOps[Byte, Byteger]          = new BoxOps(value)
  implicit def boxerShorts(value: Short): BoxOps[Short, Shortager]     = new BoxOps(value)
  implicit def intBoxOps(value: Int): BoxOps[Int, Jintejer]            = new BoxOps(value)
  implicit def longBoxOps(value: Long): BoxOps[Long, Longuage]         = new BoxOps(value)
  implicit def floatBoxOps(value: Float): BoxOps[Float, Fromage]       = new BoxOps(value)
  implicit def doubleBoxOps(value: Double): BoxOps[Double, Dégagée]    = new BoxOps(value)
}
