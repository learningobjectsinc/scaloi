package scaloiz.syntax

import java.util.concurrent.TimeUnit

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.concurrent.duration._

class FiniteDurationOpsTest extends FlatSpec with OptionValues with Matchers {
  import FiniteDurationOps._

  behavior of "FiniteDurationOps"

  it should "stringify humanly" in {
    0.millis.toHumanString should equal("no time at all")
    1.millis.toHumanString should equal("1 millisecond")
    1001.millis.toHumanString should equal("1 second, 1 millisecond")
    60001.millis.toHumanString should equal("1 minute")
    60001.millis.toFiniteDuration(TimeUnit.MINUTES) should equal(1.minute)
  }
}
