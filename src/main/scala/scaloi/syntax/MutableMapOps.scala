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

import scala.collection.mutable
import scala.language.implicitConversions
import scalaz.{Monoid, Semigroup}

/**
  * Enhancements on mutable maps.
  * @param self the mutable map
  * @tparam A the key type
  * @tparam B the value type
  */
final class MutableMapOps[A, B](val self: mutable.Map[A, B]) extends AnyVal {
  /**
    * Return a mutable map with a default value of the monoidal zero.
    * @param ev monoid evidence for the value type
    * @return the mutable map
    */
  @inline final def withDefaultZero(implicit ev: Monoid[B]): mutable.Map[A, B] = self.withDefaultValue(ev.zero)

  /**
    * Get a value from the map, if present, or else update with the monoidal zero.
    * @param a the key
    * @param ev monoid evidence for the value type
    * @return the resulting value
    */
  @inline final def getOrElseUpdateZ(a: A)(implicit ev: Monoid[B]): B = self.getOrElseUpdate(a, ev.zero)

  /**
    * Append a value to this map. If there is an existing value under the chosen key then
    * it is appended with the new value, else it is stored directly.
    * @param a the key
    * @param b the value
    * @param ev semigroup evidence for the value type
    */
  @inline final def append(a: A, b: B)(implicit ev: Semigroup[B]): Unit =
    self.update(a, self.get(a).fold(b)(ev.append(_, b)))

}

/**
  * MutableMap operations companion.
  */
object MutableMapOps extends ToMutableMapOps

/**
  * Implicit conversion for mutable map operations.
  */
trait ToMutableMapOps {

  /**
    * Implicit conversion from mutable map to the mutable map enhancements.
    * @param m the mutable map
    * @tparam A the key type
    * @tparam B the value type
    */
  implicit def toMutableMapOps[A, B](m: mutable.Map[A, B]): MutableMapOps[A, B] =
    new MutableMapOps(m)
}
