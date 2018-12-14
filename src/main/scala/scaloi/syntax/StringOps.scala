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

import scala.util.Try

/** Enhancements on strings.
  */
final class StringOps(private val self: String) extends AnyVal {

  def toBoolean_! : Try[Boolean] = Try(self.toBoolean)
  def toBoolean_? : Option[Boolean] = this.toBoolean_!.toOption

  def toLong_! : Try[Long] = Try(self.toLong)
  def toLong_? : Option[Long] = this.toLong_!.toOption

}

object StringOps extends ToStringOps

trait ToStringOps {
  import language.implicitConversions

  @inline implicit final def toStringOps(self: String): StringOps =
    new StringOps(self)
}
