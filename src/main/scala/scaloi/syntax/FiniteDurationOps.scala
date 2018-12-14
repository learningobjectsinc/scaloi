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

import java.util.concurrent.TimeUnit
import java.{time => jt}

import scala.concurrent.duration._
import scala.language.implicitConversions

import scalaz.syntax.enum._

import misc.JEnumEnum._

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
  implicit def toFiniteDurationOps(d: FiniteDuration): FiniteDurationOps = new FiniteDurationOps(d)
}
