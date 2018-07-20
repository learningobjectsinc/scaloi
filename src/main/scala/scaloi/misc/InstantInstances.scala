package scaloi
package misc

import java.time.Instant

import scalaz._

trait InstantInstances {

  implicit final val instantOrder: Order[Instant] =
    (x: Instant, y: Instant) => Ordering.fromInt(x compareTo y)
}

object InstantInstances extends InstantInstances
