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

import java.util.Map.Entry

import scala.language.implicitConversions

/**
  * Map entry pimping operations.
  *
  * @param self the entry to pimp
  * @tparam A the key type
  * @tparam B the value type
  */
final class EntryOps[A, B](private val self: Entry[A, B]) extends AnyVal {

  /**
    * Convert a map entry to a scala tuple.
    *
    * @return a tuple
    */
  def asScala: (A, B) = self.getKey -> self.getValue
}

/**
  * Implicit conversion for map entry operations.
  */
trait ToEntryOps {

  /**
    * Implicit conversion from map entry to the entry enhancements.
    * @param e the entry
    * @tparam A the key type
    * @tparam B the value type
    */
  implicit def toEntryOps[A, B](e: Entry[A, B]): EntryOps[A, B] = new EntryOps(e)
}
