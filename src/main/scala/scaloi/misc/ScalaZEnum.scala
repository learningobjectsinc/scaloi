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

package scaloi.misc

import enumeratum._
import scalaz.{Ordering, Enum => Znum}

/**
  * Mixin to provide scalaz Enum and Order typeclass evidence for enumeratum enums
  * based on their ordinal index.
  *
  * Usage:
  * {{{
  *   sealed trait Greeting extends EnumEntry
  *
  *   object Greeting extends Enum[Greeting] with ScalaZEnum[Greeting] {
  *     val values = findValues
  *     case object Hi extends Greeting
  *     case object Hello extends Greeting
  *   }
  *
  *   // Greeting.values.maximum == Some(Greeting.Hello)
  * }}}
  *
  * @tparam A the enumeration type
  */
trait ScalaZEnum[A <: EnumEntry] { self: Enum[A] =>

  implicit object EnumZnum extends Znum[A] {
    private final val n = self.values.size

    override def pred(a: A): A = self.values((self.indexOf(a) + n - 1) % n)
    override def succ(a: A): A = self.values((self.indexOf(a) + 1) % n)
    override def min: Option[A] = self.values.headOption
    override def max: Option[A] = self.values.lastOption
    override def order(x: A, y: A): Ordering =
      Ordering.fromInt(self.indexOf(x) - self.indexOf(y))
  }
}
