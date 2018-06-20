package scaloi

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scalaz.syntax.either._

class EitherNeitherBothTest extends FlatSpec with OptionValues with Matchers {
  import \|/._

  behavior of """\|/"""

  it should "match pairs of options" in {
    \|/(None, None) should matchPattern { case Neither => }
    \|/(Some(1), None) should matchPattern { case This(1) => }
    \|/(None, Some(2)) should matchPattern { case That(2) => }
    \|/(Some(1), Some(2)) should matchPattern { case Both(1, 2) => }
  }

  it should "deconstruct to either" in {
    \|/(None, None).eitherOption shouldEqual None
    \|/(Some(1), None).eitherOption shouldEqual Some(1.left)
    \|/(None, Some(2)).eitherOption shouldEqual Some(2.right)
    \|/(Some(1), Some(2)).eitherOption shouldEqual None
  }

  it should "deconstruct to this" in {
    \|/(None, None).thisOption shouldEqual None
    \|/(Some(1), None).thisOption shouldEqual Some(1)
    \|/(None, Some(2)).thisOption shouldEqual None
    \|/(Some(1), Some(2)).thisOption shouldEqual Some(1)
  }

  it should "deconstruct to that" in {
    \|/(None, None).thatOption shouldEqual None
    \|/(Some(1), None).thatOption shouldEqual None
    \|/(None, Some(2)).thatOption shouldEqual Some(2)
    \|/(Some(1), Some(2)).thatOption shouldEqual Some(2)
  }

}
