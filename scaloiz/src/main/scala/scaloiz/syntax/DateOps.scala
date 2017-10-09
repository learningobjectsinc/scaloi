package scaloiz.syntax

import java.util.Date

import scalaz._

object DateOps {
  /**
    * Order evidence for dates.
    */
  implicit val DateOrder: Order[Date] =
    (d1, d2) => Ordering.fromInt(d1 compareTo d2)
}