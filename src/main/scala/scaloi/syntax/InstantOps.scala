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

import java.time.Instant
import java.{util => ju}

final class InstantOps(private val self: Instant) extends AnyVal {

  /** Convert this [[Instant]] to a [[ju.Date Date]], truncating nanoseconds.
    *
    * @return a date object pretty close to this instant
    */
  def asDate: ju.Date = ju.Date.from(self)

}

object InstantOps extends ToInstantOps

trait ToInstantOps {
  import language.implicitConversions

  @inline implicit final def ToInstantOps(self: Instant): InstantOps =
    new InstantOps(self)
}
