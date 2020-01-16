package scaloi
package syntax

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scaloi.test.ScaloiTest

import scala.util.{Failure, Success}

class StringOpsTest
  extends AnyFlatSpec
     with Matchers
     with ScaloiTest
{

  behaviour of "StringOps"
  import string._

  it should "optionally parse longs" in {
    "1234".toLong_? should be (Some(1234L))
    "one thousand two hundred thirty four".toLong_? should be (None)
  }

  it should "trepidatiously parse longs" in {
    "99".toLong_! should be (Success(99))
    "ninety-nine".toLong_! should matchPattern {
      case Failure(_: NumberFormatException) =>
    }
  }
  it should "optionally parse booleans" in {
    "true".toBoolean_? should be (Some(true))
    "verily".toBoolean_? should be (None)
  }

  it should "cautiously parse longs" in {
    "false".toBoolean_! should be (Success(false))
    "inconceivable".toBoolean_! should matchPattern {
      case Failure(_: IllegalArgumentException) =>
    }
  }

}
