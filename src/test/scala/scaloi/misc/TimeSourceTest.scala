package scaloi.misc

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class TimeSourceTest extends FlatSpec with OptionValues with Matchers {
  behavior of "TimeSource"

  it should "source time" in {
    implicitly[TimeSource].time should equal(System.currentTimeMillis +- 1000L)
  }

}
