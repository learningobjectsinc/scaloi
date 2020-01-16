package scaloi.misc

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TimeSourceTest extends AnyFlatSpec with OptionValues with Matchers {
  behavior of "TimeSource"

  it should "source time" in {
    implicitly[TimeSource].time should equal(System.currentTimeMillis +- 1000L)
  }

}
