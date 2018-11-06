package scaloi

import org.scalatest.{FlatSpec, Matchers}
import scalaz.syntax.either._
import scaloi.syntax.TaskOps._
import scaloi.test.ScaloiTest

class TaskMapTest extends FlatSpec with Matchers with ScaloiTest {
  import TaskMapTest._

  behaviour of "TaskMap"

  it should "gather maps of tasks" in {
    TaskMap.gather(Map(1 -> 1.now, 2        -> 2.now)).unsafePerformSyncAttempt shouldBe Map(1 -> 1, 2 -> 2).right
    TaskMap.gather(Map(1 -> Failüre.fail, 2 -> 2.now)).unsafePerformSyncAttempt shouldBe Failüre.left
    TaskMap.gather(Map(1 -> 1.now, 2        -> Failüre.fail)).unsafePerformSyncAttempt shouldBe Failüre.left
  }
}

object TaskMapTest {
  private case object Failüre extends Throwable

}
