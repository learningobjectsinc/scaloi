package scaloi

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaz.syntax.either._

class EitherNeitherBothTest extends AnyFlatSpec with OptionValues with Matchers {
  import \|/._

  behavior of """\|/"""

  it should "match pairs of options" in {
    \|/(Option.empty[Int], Option.empty[Int]) should matchPattern { case Neither() => }
    \|/(Some(1), Option.empty[Int]) should matchPattern { case This(1) => }
    \|/(Option.empty[Int], Some(2)) should matchPattern { case That(2) => }
    \|/(Some(1), Some(2)) should matchPattern { case Both(1, 2) => }
  }

  it should "deconstruct to either" in {
    \|/(Option.empty[Int], Option.empty[Int]).eitherOption shouldEqual None
    \|/(Some(1), Option.empty[Int]).eitherOption shouldEqual Some(1.left)
    \|/(Option.empty[Int], Some(2)).eitherOption shouldEqual Some(2.right)
    \|/(Some(1), Some(2)).eitherOption shouldEqual None
  }

  it should "deconstruct to this" in {
    \|/(Option.empty[Int], Option.empty[Int]).thisOption shouldEqual None
    \|/(Some(1), Option.empty[Int]).thisOption shouldEqual Some(1)
    \|/(Option.empty[Int], Some(2)).thisOption shouldEqual None
    \|/(Some(1), Some(2)).thisOption shouldEqual Some(1)
  }

  it should "deconstruct to that" in {
    \|/(Option.empty[Int], Option.empty[Int]).thatOption shouldEqual None
    \|/(Some(1), Option.empty[Int]).thatOption shouldEqual None
    \|/(Option.empty[Int], Some(2)).thatOption shouldEqual Some(2)
    \|/(Some(1), Some(2)).thatOption shouldEqual Some(2)
  }

}
