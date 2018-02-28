package scaloi.syntax

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{CyclicBarrier, Executors}

import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class LockOpsTest extends FlatSpec with Matchers with BeforeAndAfterAll {
  import LockOps._

  private val es                            = Executors.newCachedThreadPool()
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)

  override def afterAll(): Unit = {
    es.shutdown()
    super.afterAll()
  }

  behavior of "LockOps"

  it should "lock" in {
    var count   = 0
    val lock    = new ReentrantLock()
    val barrier = new CyclicBarrier(2)
    Await.result(
      Future.sequence(
        List(
          Future {
            lock locked {
              barrier.await()
              val c = count
              Thread.sleep(500)
              count = c + 1
              count
            }
          },
          Future {
            barrier.await()
            lock locked {
              // this cannot execute until the previous lock has completed
              count = count + 1
              count
            }
          }
        )),
      5.seconds
    ) should be(List(1, 2))
  }
}
