package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

class EitherOpsTest extends FlatSpec with OptionValues with Matchers {
  import EitherOps._

  behavior of "EitherOps"

  it should "tap into values" in {
    var state = 0
    Right(1) <|- { s =>
      state = s
    } should equal(Right(1))
    state should equal(1)
    Left(2) <|- { s: Int =>
      state = s
    } should equal(Left(2))
    state should equal(1)
    Right(3).rightTap(s => state = s) should equal(Right(3))
    state should equal(3)

    Right(4) -<| { s: Int =>
      state = s
    } should equal(Right(4))
    state should equal(3)
    Left(5) -<| { s =>
      state = s
    } should equal(Left(5))
    state should equal(5)
    Left(6).leftTap(s => state = s) should equal(Left(6))
    state should equal(6)
  }

  it should "left map" in {
    Left[Int, Int](3).leftMap(_ * 2) should equal(Left(6))
    Right[Int, Int](3).leftMap(_ * 2) should equal(Right(3))
  }

  it should "value or" in {
    Left[Int, Int](3).valueOr(_ * 2) should equal(6)
    Right[Int, Int](3).valueOr(_ * 2) should equal(3)
  }
}
