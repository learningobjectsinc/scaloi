package scaloi.syntax

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.lang.Thread.State

class JEnumOpsTest extends AnyFlatSpec with OptionValues with Matchers {
  import enum._

  behavior of "JEnumOps"

  it should "compare java enum" in {
    State.NEW < State.TERMINATED should equal(true)
    State.TERMINATED < State.NEW should equal(false)

    State.NEW > State.TERMINATED should equal(false)
    State.TERMINATED > State.NEW should equal(true)
  }

}
