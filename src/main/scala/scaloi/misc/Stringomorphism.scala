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
package misc

import java.time.Instant

import enumeratum.EnumEntry
import scaloi.syntax.StringOps._

import scala.util.{Success, Try}

/** A stringomorphism represents a transformation from a string
  * to a type, or an error if the transformation falise.
  * @tparam T the target type
  */
trait Stringomorphism[T] extends (String => Try[T])

object Stringomorphism {
  def apply[T](implicit st: Stringomorphism[T]): Stringomorphism[T] = st

  /** A homomomorphism for strings. */
  implicit val Stringhomorphism: Stringomorphism[String] = s => Success(s)

  /** A stringomorphism to an [[Instant]]. */
  implicit val InstantStringomorphism: Stringomorphism[Instant] = s => Try(Instant.parse(s))

  /** A stringomorphism to a [[Boolean]]. */
  implicit val BooleanStringomorphism: Stringomorphism[Boolean] = s => Try(s.toBoolean)

  /** A stringomorphism to a [[Long]]. */
  implicit val LongStringomorphism: Stringomorphism[Long] = _.toLong_!

  /** A stringomorphism to an enumeratum [[EnumEntry]]. */
  implicit def enumeratumStringomorphism[E <: EnumEntry: Enumerative]: Stringomorphism[E] =
    s => Try(Enumerative[E].enum.withName(s))
}
