package scaloi.data

import scaloi.misc.TimeSource
import org.scalatest.{FlatSpec, Matchers, OptionValues}

class BucketGenerationalDedupTest extends FlatSpec with OptionValues with Matchers {
  behavior of "BucketGenerationalDedup"

  it should "dedup" in {
    implicit object ts extends TimeSource {
      var time: Long = 0L
    }
    val dedup = new BucketGenerationalDedup[Int](300)
    dedup += 1
    dedup.contains(1) should equal(true)
    dedup.contains(2) should equal(false)
    dedup.bucketCount should equal(1)
    ts.time = 50L
    dedup += 1
    dedup.bucketCount should equal(1)
    ts.time = 100L
    dedup += 2
    dedup.contains(1) should equal(true)
    dedup.contains(2) should equal(true)
    dedup.bucketCount should equal(2)
    ts.time = 200L
    dedup += 2
    dedup.contains(1) should equal(true)
    dedup.contains(2) should equal(true)
    dedup.bucketCount should equal(3)
    ts.time = 300L
    dedup ++= Seq(3)
    dedup.contains(1) should equal(true)
    dedup.contains(3) should equal(true)
    dedup.bucketCount should equal(4)
    ts.time = 500L
    dedup.contains(1) should equal(false)
    dedup.contains(2) should equal(true)
    dedup.contains(3) should equal(true)
    dedup ++= Seq(3)
    dedup.bucketCount should equal(3)
  }

}
