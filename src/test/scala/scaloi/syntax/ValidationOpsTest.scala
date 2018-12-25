package scaloi.syntax

import org.scalatest.{FlatSpec, Matchers}
import scaloi.test.ScaloiTest

class ValidationOpsTest extends FlatSpec
  with ScaloiTest
  with Matchers {

  import validation._
  import scalaz.syntax.validation._

  import scala.util.{Success, Failure}

  behavior of "ValidationOps"

  case class MistakeException(msg: String = "Uh-oh") extends Throwable

  it should "convert to a Try" in {
    5.successNel[Throwable].toTry(_ => MistakeException()) shouldBe Success(5)
    5.failureNel[Throwable].toTry(_ => MistakeException()) shouldBe Failure(MistakeException())
    5.success[Throwable].toTry(_ => MistakeException()) shouldBe Success(5)
    5.failure[Throwable].toTry(_ => MistakeException()) shouldBe Failure(MistakeException())
  }

}
