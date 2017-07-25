package scaloi.syntax

import java.util.concurrent.TimeUnit
import java.{time => jt}

import scaloi.misc.JEnumEnum._

import scala.concurrent.duration._
import scala.language.implicitConversions
import scalaz.syntax.enum._

/**
  * Finite duration pimping operations.
  *
  * @param self the duration to pimp
  */
final class FiniteDurationOps(val self: FiniteDuration) extends AnyVal {

  /** Return a human string representation of this duration.
    * It is scaled to the largest non-zero time unit and then
    * the value is printed in terms of that time unit and the
    * next smaller one. For example "1 minute, 3 seconds".
    */
  def toHumanString: String = {
    largestUnit.fold("no time at all") { u =>
      val scaled = toFiniteDuration(u)
      u.predx.fold(scaled.toString) { v =>
        val modulus   = FiniteDuration(1, u).toUnit(v).toInt
        val remainder = self.toUnit(v).toLong % modulus
        if (remainder > 0)
          scaled + ", " + FiniteDuration(remainder, v)
        else
          scaled.toString
      }
    }
  }

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
  private def largestUnit: Option[TimeUnit] =
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
