package scaloi
package syntax

import java.sql.Timestamp
import java.util.Date

import scalaz._
import scala.concurrent.duration._
import scala.language.implicitConversions

/**
  * Enhancements on dates.
  * @param d the date
  */
final class DateOps(val d: Date) extends AnyVal {

  /**
    * Add a duration to a date.
    * @param s the duration
    * @return the new date
    */
  def +(s: Duration): Date = new Date(d.getTime + s.toMillis)

  /**
    * Subtract a duration from a date.
    * @param s the duration
    * @return the new date
    */
  def -(s: Duration): Date = new Date(d.getTime - s.toMillis)

  /**
    * Subtract a date from a date.
    * @param other the other date
    * @return the difference between the dates
    */
  def -(other: Date): FiniteDuration = (d.getTime - other.getTime).millis

  /**
    * Return how far this date is from now.
    * @param ts a time source
    * @return the duration from now to this date
    */
  def fromNow(implicit ts: misc.TimeSource): FiniteDuration = this.-(ts.date)

  /**
    * Convert to a [[Timestamp]].
    * @return this date as a [[Timestamp]]
    */
  def toTimestamp: Timestamp = new Timestamp(d.getTime)
}

/**
  * Date operations companion.
  */
object DateOps extends ToDateOps {

  /**
    * Order evidence for dates.
    */
  implicit val DateOrder: Order[Date] =
    (d1, d2) => Ordering.fromInt(d1 compareTo d2)
}

/**
  * Implicit conversion for date operations.
  */
trait ToDateOps {

  /**
    * Implicit conversion from a date to enhancements.
    * @param date the date
    */
  implicit def toDateOps(date: Date): DateOps = new DateOps(date)
}
