package scaloi.syntax

import java.util.Date

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.concurrent.duration._

class DateOpsTest extends FlatSpec with OptionValues with Matchers {
  import DateOps._

  behavior of "DateOps"

  it should "perform arithmetic on dates" in {
    val date = new Date(100000L)
    date + 10.seconds should equal(new Date(110000L))
    date - 10.seconds should equal(new Date(90000L))
    (date - new Date(1000L)) should equal(99.seconds)
  }
}
