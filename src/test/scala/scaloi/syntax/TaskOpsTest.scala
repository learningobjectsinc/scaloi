package scaloi
package syntax

import org.scalatest.{FlatSpec, Matchers}
import scaloi.test.ScaloiTest
import scalaz.syntax.either._
import scala.concurrent.duration.Duration
import scala.concurrent.Await

class TaskOpsTest extends FlatSpec with Matchers with ScaloiTest {
  import task._
  import TaskOpsTest._

  behaviour of "TaskOps"

  it should "task values now" in {
    1.now.unsafePerformSyncAttempt shouldBe 1.right
  }

  it should "task failures" in {
    Failüre.fail.unsafePerformSyncAttempt shouldBe Failüre.left
  }

  it should "perform tasks in the year 2000" in {
    Await.result(1.now.unsafePerformFuture, Duration.Inf) shouldBe 1
    the[Failüre.type] thrownBy Await.result(Failüre.fail.unsafePerformFuture, Duration.Inf) shouldBe Failüre
  }

}

object TaskOpsTest {
  private case object Failüre extends Throwable

}
