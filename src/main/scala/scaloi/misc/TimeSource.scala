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

import java.sql.Timestamp
import java.time.Instant
import java.util.Date

/** Abstraction over the current time. */
trait TimeSource {

  /** Get the current time in milliseconds. */
  def time: Long

  /** Get the current time as a date. */
  def date: Date = new Date(time) // do this with TimeFormat magnets?

  /** Get the current time as a timestamp. */
  def timestamp: Timestamp = new Timestamp(time)

  /** Get the current time as an instant. */
  def instant: Instant = date.toInstant

  def <[T: TimeyWimey](t: T): Boolean  = time < TimeyWimey[T].time(t)
  def <=[T: TimeyWimey](t: T): Boolean = time <= TimeyWimey[T].time(t)
  def >=[T: TimeyWimey](t: T): Boolean = time >= TimeyWimey[T].time(t)
  def >[T: TimeyWimey](t: T): Boolean  = time > TimeyWimey[T].time(t)
}

/** Time source companion. */
object TimeSource {

  /** Evidence of `System` as a source of truth for the time. */
  implicit def ts: TimeSource = new TimeSource() {
    override val time: Long = System.currentTimeMillis
  }

  def fromInstant(instant0: Instant): TimeSource = new TimeSource() {
    override val time: Long = instant0.toEpochMilli
  }
}

trait TimeyWimey[T] {
  def time(t: T): Long
}

object TimeyWimey {
  def apply[T](implicit T: TimeyWimey[T]): TimeyWimey[T]  = T
  implicit val DateTimeyWimey: TimeyWimey[Date]           = _.getTime
  implicit val TimestampTimeyWimey: TimeyWimey[Timestamp] = _.getTime
  implicit val InstantTimeyWimey: TimeyWimey[Instant]     = _.toEpochMilli

}
