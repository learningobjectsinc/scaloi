package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}


import scala.util.{Failure, Success}

class OptionOpsTest extends FlatSpec with OptionValues with Matchers {
  import OptionOps._

  behavior of "OptionOps"

  it should "tap options" in {
    var state = 0
    Some(1) <|? { s =>
      state = s
    } should equal(Some(1))
    state should equal(1)
    None <|? { s: Int =>
      state = 2
    } should equal(None)
    state should equal(1)
    Some(3).tap(state = _) should equal(Some(3))
    state should equal(3)
  }

  it should "futurize options" in {
    val ex = new Exception

    Some(1).toFuture(ex).value.value should equal(Success(1))
    None.toFuture(ex).value.value should equal(Failure(ex))
  }

  it should "subtract options" in {
    Some(1) - 1 should equal(None)
    Some(1) - 2 should equal(Some(1))
    None - 3 should equal(None)
  }

  it should "optionally transform" in {
    Some((_: Int) * 2) transforming 5 should equal(10)
    None transforming "Bolivia" should equal("Bolivia")

    Some((s: String) => s"${s}inator") ~?> "option" should equal("optioninator")
    None ~?> "King Wiggle-Wut the Greater" should equal(
      "King Wiggle-Wut the Greater")
  }
}
