package scaloi.data

import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class UnboundedBlockingFairKeyedQueueTest extends FlatSpec with OptionValues with Matchers {
  behavior of "UnboundedBlockingFairKeyedQueue"

  it should "be fair" in {
    val queue = UnboundedBlockingFairKeyedQueue.empty[String, String]
    queue.offer("A", "a1")
    queue.offer("A", "a2")
    queue.offer("A", "a3")
    queue.offer("B", "b1")
    queue.offer("B", "b2")
    queue.offer("C", "c1")
    val results = 1 to 6 map { _ => queue.take() }
    results.toList should equal(List("a1", "b1", "c1", "a2", "b2", "a3"))
  }

  it should "be structural" in {
    val queue = UnboundedBlockingFairKeyedQueue.empty[String, String]
    queue.offer("A", "a1")
    queue.offer("A", "a2")
    queue.offer("A", "a3")
    queue.offer("B", "b1")
    queue.offer("B", "b2")
    queue.offer("C", "c1")
    queue.size should equal(6)
    queue.isEmpty should equal(false)
    queue.toMap should equal(Map("A" -> List("a1", "a2", "a3"), "B" -> List("b1", "b2"), "C" -> List("c1")))
    queue.take()
    queue.size should equal(5)
    queue.clear()
    queue.size should equal(0)
    queue.nonEmpty should equal(false)
  }

  it should "block" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val queue = UnboundedBlockingFairKeyedQueue.empty[String, String]
    val future = Future { queue.takeTuple() }
    Thread.sleep(1000)
    queue.offer("A", "a1")
    Await.result(future, 1.minute) should equal("A" -> "a1")
    queue.size should equal(0)
  }

  it should "wait with timeout" in {
    import scala.concurrent.ExecutionContext.Implicits.global

    val queue = UnboundedBlockingFairKeyedQueue.empty[String, String]
    val future = Future { queue.takeTuple(500.millis) }
    Await.result(future, 1.second) should equal (None)
    queue.size should equal (0)

    val future1 = Future { queue.takeTuple(500.millis) }
    Thread.sleep(100)
    queue.offer("B", "b1")
    Await.result(future1, 1.second) should equal (Some("B" -> "b1"))
  }

}
