package scaloi
package syntax

import java.util.concurrent.TimeUnit
import java.{time => jt}

import scala.concurrent.duration._
import scala.language.implicitConversions

/**
  * Finite duration pimping operations.
  *
  * @param self the duration to pimp
  */
class FiniteDurationOps(self: FiniteDuration) {

  /** Convert this to the specified time unit, discarding
    * any fractional part.
    * @param tu the target time unit
    * @return the new finite duration
    */
  def toFiniteDuration(tu: TimeUnit): FiniteDuration =
    FiniteDuration(self.toUnit(tu).toLong, tu)

  /** Convert this to a java.time.Duration.
    * @return a java.time.Duration expressing the same value
    */
  def asJava: jt.Duration = {
    val seconds = self.toSeconds
    val nanos =
      if (seconds > 0) (self - seconds.seconds).toNanos
      else if (seconds < 0) (self + seconds.abs.seconds).toNanos
      else self.toNanos
    jt.Duration.ofSeconds(seconds, nanos)
  }

  /** Find the largest time unit contained by this duration. */
  protected def largestUnit: Option[TimeUnit] =
    TimeUnit.values.reverse.find(u => self.toUnit(u) >= 1.0)
}

/**
  * Finite duration operations companion.
  */
object FiniteDurationOps extends ToFiniteDurationOps

/**
  * Implicit conversion for Finite duration operations.
  */
trait ToFiniteDurationOps {

  /**
    * Implicit conversion from finite duration to the duration enhancements.
    * @param d the finite duration
    */
  implicit def toFiniteDurationOps(d: FiniteDuration): FiniteDurationOps = new FiniteDurationOps(d)
}
