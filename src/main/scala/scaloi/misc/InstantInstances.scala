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

import java.time.{Instant, ZonedDateTime}

import scalaz._

trait InstantInstances {

  implicit final val instantOrder: Order[Instant] =
    (x: Instant, y: Instant) => Ordering.fromInt(x compareTo y)

  implicit final val zdtOrder: Order[ZonedDateTime] =
    (x: ZonedDateTime, y: ZonedDateTime) => Ordering.fromInt(x.toInstant compareTo y.toInstant)
}

object InstantInstances extends InstantInstances
