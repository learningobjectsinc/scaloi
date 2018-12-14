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

import scala.language.implicitConversions

/**
  * Enhancements on sets.
  *
  * @param self the set
  * @tparam A the set value type
  */
final class SetOps[A](val self: Set[A]) extends AnyVal {

  /**
    * Convert this set to a map with a function from keys to values.
    *
    * @param f a function from keys to values
    * @tparam B the value type
    * @return the resulting map
    */
  def mapTo[B](f: A => B): Map[A, B] = self.map(a => a -> f(a)).toMap
}

/**
  * Set operations companion.
  */
object SetOps extends ToSetOps

/**
  * Implicit conversion for set tag operations.
  */
trait ToSetOps {

  /**
    * Implicit conversion from set to the set enhancements.
    * @param c the set
    * @tparam C its type
    */
  implicit def toSetOps[C](c: Set[C]): SetOps[C] = new SetOps(c)
}
