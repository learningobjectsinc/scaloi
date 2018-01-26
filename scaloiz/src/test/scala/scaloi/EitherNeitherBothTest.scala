package scaloi

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scaloiz.\|/

class EitherNeitherBothTest extends FlatSpec with OptionValues with Matchers {
  import scaloiz.\|/._

  behavior of """\|/"""

  it should "match pairs of options" in {
    \|/(None, None) should matchPattern { case Neither => }
    \|/(Some(1), None) should matchPattern { case This(1) => }
    \|/(None, Some(2)) should matchPattern { case That(2) => }
    \|/(Some(1), Some(2)) should matchPattern { case Both(1, 2) => }
  }

}
