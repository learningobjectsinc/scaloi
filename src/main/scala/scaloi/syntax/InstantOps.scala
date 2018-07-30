package scaloi
package syntax

import java.time.Instant
import java.{util => ju}

final class InstantOps(private val self: Instant) extends AnyVal {

  /** Convert this [[Instant]] to a [[ju.Date Date]], truncating nanoseconds.
    *
    * @return a date object pretty close to this instant
    */
  def asDate: ju.Date = ju.Date.from(self)

}

object InstantOps extends ToInstantOps

trait ToInstantOps {
  import language.implicitConversions

  @inline implicit final def ToInstantOps(self: Instant): InstantOps =
    new InstantOps(self)
}
