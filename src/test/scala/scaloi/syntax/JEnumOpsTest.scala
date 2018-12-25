package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import java.lang.Thread.State

class JEnumOpsTest extends FlatSpec with OptionValues with Matchers {
  import enum._

  behavior of "JEnumOps"

  it should "compare java enum" in {
    State.NEW < State.TERMINATED should equal(true)
    State.TERMINATED < State.NEW should equal(false)

    State.NEW > State.TERMINATED should equal(false)
    State.TERMINATED > State.NEW should equal(true)
  }

}
