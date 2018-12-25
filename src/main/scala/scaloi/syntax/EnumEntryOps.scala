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

import _root_.enumeratum.EnumEntry

final class EnumEntryOps[E <: EnumEntry](private val self: E) extends AnyVal {

  /** Summon the enum companion object for this enum entry.
    */
  @inline
  def enum(implicit ee: misc.Enumerative[E]): ee.enum.type = ee.enum

}

trait ToEnumEntryOps {
  import language.implicitConversions

  @inline
  implicit final def ToEnumEntryOps[E <: EnumEntry](self: E): EnumEntryOps[E] =
    new EnumEntryOps[E](self)

}
