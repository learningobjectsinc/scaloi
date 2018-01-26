package scaloiz.syntax

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.util.{Failure, Success}



class EitherOpsTest extends FlatSpec with OptionValues with Matchers {
  import EitherOps._

  behavior of "EitherOps"

  it should "get values when nothing is thrown" in {
    treither(2).orThrow should equal(2)
  }

  it should "throw exceptions when nothing is there" in {
    val ex = new Exception
    the[Exception] thrownBy treither(throw ex).orThrow should equal(ex)
  }

  it should "get successes when nothing is thrown" in {
    treither(2).toTry should equal(Success(2))
  }

  it should "get failures when nothing is there" in {
    val ex = new Exception
    treither(throw ex).toTry should equal(Failure(ex))
  }

}
