package scaloi.data

import java.util.concurrent.Executors

import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers, OptionValues}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

class UnboundedBlockingFairKeyedQueueTest extends FlatSpec with OptionValues with Matchers with BeforeAndAfterAll {

  private val es = Executors.newFixedThreadPool(1)
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)

  override def afterAll(): Unit = {
    es.shutdown()
    super.afterAll()
  }

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
    val queue = UnboundedBlockingFairKeyedQueue.empty[String, String]
    val future = Future { queue.takeTuple() }
    Thread.sleep(1000)
    queue.offer("A", "a1")
    Await.result(future, 5.seconds) should equal("A" -> "a1")
    queue.size should equal(0)
  }

  it should "wait with timeout" in {
    val queue = UnboundedBlockingFairKeyedQueue.empty[String, String]
    val future = Future { queue.takeTuple(500.millis) }
    Await.result(future, 5.seconds) should equal (None)
    queue.size should equal (0)

    val future1 = Future { queue.takeTuple(5000.millis) }
    Thread.sleep(100)
    queue.offer("B", "b1")
    Await.result(future1, 15.seconds) should equal (Some("B" -> "b1"))
  }

}
