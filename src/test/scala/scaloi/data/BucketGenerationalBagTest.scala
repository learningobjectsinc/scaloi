package scaloi.data

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scaloi.misc.TimeSource

import scala.concurrent.duration._

class BucketGenerationalBagTest extends AnyFlatSpec with Matchers {

  behavior of "BucketGenerationalBag"

  it should "win bagly" in {
    val time = new MutableTimeSource(0L)
    val bag = BucketGenerationalBag.empty[String](300.millis, 3)(time)

    bag.add("B")
    bag.add("A")
    time.time = 100
    bag.add("A")
    bag.add("A")
    time.time = 200
    bag.add("A")
    bag.add("C")
    time.time = 300
    bag.add("A")
    bag.add("D")

    bag.count("B") should be(0)
    bag.count("A") should be(4)
    bag.count("C") should be(1)
    bag.count("D") should be(1)
    bag.count("A", 100.millis) should be(1)
    bag.count("D", 100.millis) should be(1)
    bag.count("C", 100.millis) should be(0)
    bag.count("A", 150.millis) should be(2)
    bag.count("D", 150.millis) should be(1)
    bag.count("C", 150.millis) should be(1)
    bag.bucketCount should be(3)
  }
}

class MutableTimeSource(var time: Long) extends TimeSource
