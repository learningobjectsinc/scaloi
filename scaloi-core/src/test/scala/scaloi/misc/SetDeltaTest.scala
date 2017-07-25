package scaloi.misc

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class SetDeltaTest extends FlatSpec with OptionValues with Matchers {
  behavior of "SetDelta"

  it should "delta sets" in {
    SetDelta from Set(1) to Set(1) should equal(SetDelta(Set.empty, Set.empty))
    SetDelta from Set(1) to Set(2) should equal(SetDelta(Set(2), Set(1)))
    SetDelta from Set(1,2) to Set(2,3) should equal(SetDelta(Set(3), Set(1)))
  }
}
