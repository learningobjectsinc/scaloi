package scaloi
package misc

import java.time.{Instant, ZonedDateTime}

import scalaz._

trait InstantInstances {

  implicit final val instantOrder: Order[Instant] =
    (x: Instant, y: Instant) => Ordering.fromInt(x compareTo y)

  implicit final val zdtOrder: Order[ZonedDateTime] =
    (x: ZonedDateTime, y: ZonedDateTime) => Ordering.fromInt(x.toInstant compareTo y.toInstant)
}

object InstantInstances extends InstantInstances
