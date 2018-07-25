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
  def timestamp: Timestamp = new Timestamp((time))

  /** Get the current time as an instant. */
  def instant: Instant = date.toInstant
}

/** Time source companion. */
object TimeSource {
  /** Evidence of `System` as a source of truth for the time. */
  implicit val ts: TimeSource = new TimeSource() {
    override def time: Long = System.currentTimeMillis
  }
}
