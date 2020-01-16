package scaloi.syntax

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SetOpsTest extends AnyFlatSpec with OptionValues with Matchers {
  import set._

  behavior of "SetOps"

  it should "map sets" in {
    Set(1, 2, 3).mapTo(_ * 2) should equal(Map((1, 2), (2, 4), (3, 6)))
    Set(1, 2, 3).map(a => (a, a * 2)).toMap should equal(Map((1, 2), (2, 4), (3, 6)))
  }

}
