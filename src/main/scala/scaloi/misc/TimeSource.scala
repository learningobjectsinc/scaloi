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
  implicit val ts: TimeSource = new TimeSource() {
    override def time: Long = System.currentTimeMillis
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
