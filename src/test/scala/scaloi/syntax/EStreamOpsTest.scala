package scaloi.syntax

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scalaz.EphemeralStream._

class EStreamOpsTest extends AnyFlatSpec with OptionValues with Matchers {
  import eStream._

  behavior of "EStreamOps"

  it should "nonempty" in {
    ø.nonEmpty shouldBe false
    (1 ##:: ø).nonEmpty shouldBe true
  }

  it should "foreach" in {
    var a = 0
    0 ##:: 1 ##::2 ##:: ø foreach { a += _ }
    a shouldEqual 3
  }

  val ø = emptyEphemeralStream[Int]
}
