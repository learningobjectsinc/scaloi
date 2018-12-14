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

import java.lang.{Enum => JEnum}

import scala.language.implicitConversions

/**
  * Enhancements on java enums.
  *
  * @param self the enumeration value
  * @tparam A the enumeration type
  */
final class JEnumOps[A <: JEnum[A]](val self: A) extends AnyVal {

  /**
    * Compare enums by ordinal value.
    *
    * @param b the other enum value
    * @return if this enum value is less
    */
  @inline final def <(b: A): Boolean = self.ordinal < b.ordinal

  /**
    * Compare enums by ordinal value.
    *
    * @param b the other enum value
    * @return if this enum value is less
    */
  @inline final def >(b: A): Boolean = self.ordinal > b.ordinal
}

/**
  * Java enum operations companion.
  */
object JEnumOps extends ToJEnumOps

/**
  * Implicit conversion for class tag operations.
  */
trait ToJEnumOps {

  /**
    * Implicit conversion from enum to the enum enhancements.
    * @param e the enum value
    * @tparam A its type
    */
  implicit def toJEnumOps[A <: JEnum[A]](e: A): JEnumOps[A] = new JEnumOps(e)
}
