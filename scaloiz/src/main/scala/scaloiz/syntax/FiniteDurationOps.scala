package scaloiz.syntax

import scaloiz.misc.JEnumEnum._

import scalaz.syntax.enum._
import scaloi.syntax.{FiniteDurationOps => CoreOps}

import scala.concurrent.duration.FiniteDuration


final class FiniteDurationOps(self: FiniteDuration) extends CoreOps(self) {
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
  implicit def toFiniteDurationOpz(d: FiniteDuration): FiniteDurationOps = new FiniteDurationOps(d)
}
