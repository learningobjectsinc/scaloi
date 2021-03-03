package scaloi.syntax

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaz.NonEmptyList
import scaloi.test.ScaloiTest

class ValidationOpsTest extends AnyFlatSpec
  with ScaloiTest
  with Matchers {

  import validation._
  import scalaz.syntax.validation._

  import scala.util.{Success, Failure}

  behavior of "ValidationOps"

  case object MistakeException extends Throwable

  it should "convert to a Try" in {
    5.successNel[String].toTry(_ => MistakeException) shouldBe Success(5)
    5.failureNel.toTry(_ => MistakeException) shouldBe Failure(MistakeException)
    5.success[String].toTry(_ => MistakeException) shouldBe Success(5)
    5.failure.toTry(_ => MistakeException) shouldBe Failure(MistakeException)
  }

  it should "validate things" in {
    import scalaz.{Failure, Success}

    "true".validWhen(_.toBoolean, "Error") shouldBe Success("true")
    "false".validWhen(_.toBoolean, "Error") shouldBe Failure("Error")
    "false".validUnless(_.toBoolean, "Error") shouldBe Success("false")
    "true".validUnless(_.toBoolean, "Error") shouldBe Failure("Error")

    "true".validNelWhen(_.toBoolean, "Error") shouldBe Success("true")
    "false".validNelWhen(_.toBoolean, "Error") shouldBe Failure(NonEmptyList("Error"))
    "false".validNelUnless(_.toBoolean, "Error") shouldBe Success("false")
    "true".validNelUnless(_.toBoolean, "Error") shouldBe Failure(NonEmptyList("Error"))
  }

}
