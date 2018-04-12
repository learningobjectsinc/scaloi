package scaloi.misc

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scaloi.misc.TryInstances._

import scala.util.{Failure, Success, Try}

class TryInstancesTest extends FlatSpec with OptionValues with Matchers {
  "Try List" should "fail fast" in {
    val values: List[Int]               = List(0, 1, 2, 3)
    val triedValues: Try[Iterable[Int]] = values.traverseTryListFF(lessThanOne)

    triedValues match {
      case Failure(iae: IllegalArgumentException) => iae.getMessage shouldBe "I don't like 1"
      case _                                      => fail("Should have been Failure")
    }
  }

  it should "should succeed" in {
    val values: List[Int]          = List(0, 0, 0)
    val triedValues: Try[Seq[Int]] = values.traverseTryListFF(lessThanOne)

    triedValues match {
      case Success(ints) => ints.size shouldBe 3
      case _             => fail("Should have succeeded")
    }
  }

  def lessThanOne(x: Int): Try[Int] = {
    if (x < 1) {
      Success(x)
    } else if (x == 1) {
      Failure(new IllegalArgumentException("I don't like 1"))
    } else {
      Failure(new IllegalArgumentException("I don't like large numbers"))
    }
  }
}
