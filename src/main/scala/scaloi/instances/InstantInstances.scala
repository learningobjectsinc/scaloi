package scaloi
package instances

import java.time.Instant

import scalaz._

trait InstantInstances {

  implicit final val instantOrder: Order[Instant] =
    new Order[Instant] {
      def order(x: Instant, y: Instant): Ordering =
        Ordering.fromInt(x compareTo y)
    }
}

object instant extends InstantInstances
