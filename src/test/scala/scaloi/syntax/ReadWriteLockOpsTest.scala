package scaloi.syntax

import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.{CyclicBarrier, Executors}

import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class ReadWriteLockOpsTest extends FlatSpec with Matchers with BeforeAndAfterAll {
  import readWriteLock._

  private val es                            = Executors.newCachedThreadPool()
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)

  override def afterAll(): Unit = {
    es.shutdown()
    super.afterAll()
  }

  behavior of "ReadWriteLockOps"

  it should "read-write lock" in {
    var count0  = 0
    var count1  = 0
    val lock    = new ReentrantReadWriteLock()
    val barrier = new CyclicBarrier(3)
    Await.result(
      Future.sequence(
        List(
          Future {
            lock reading {
              barrier.await()
              Thread.sleep(500)
              count0 = count0 + 1
              count0
            }
          },
          Future {
            lock reading {
              barrier.await()
              Thread.sleep(500)
              count1 = count1 + 1
              count1
            }
          },
          Future {
            barrier.await()
            lock writing {
              count0 + count1
            }
          }
        )),
      5.seconds
    ) should be(List(1, 1, 2))
  }
}
