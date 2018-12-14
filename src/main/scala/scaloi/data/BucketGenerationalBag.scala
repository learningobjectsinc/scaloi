/*
 * Copyright 2007 Cengage Learning, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scaloi.data

import scaloi.misc.TimeSource
import scaloi.syntax.AnyOps._
import scaloi.syntax.MutableMapOps._

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scalaz.std.anyVal.intInstance
import scalaz.std.list._
import scalaz.syntax.foldable._

/**
  * Counts approximately how many items have been added within a given time window.
  *
  * This class is not threadsafe.
  *
  * @tparam A the element type
  * @param expiration the duration for which to retain items
  * @param buckets the number of buckets
  * @param ts the time source
  */
class BucketGenerationalBag[A](expiration: FiniteDuration, buckets: Int, ts: TimeSource) {
  import BucketGenerationalBag._

  /** Bags of keys. */
  private[this] var bags = List.empty[BagBucket[A]]

  /** Validity window for the active (head) bucket. */
  private[this] val active = expiration / buckets.toLong

  /** Add a key to this bag. */
  def add(key: A): Unit = head.add(key)

  /** Get the approximate number of instances of a key in this bag. */
  def count(key: A): Int = bags.filter(valid(_, expiration)).foldMap(_.count(key))

  /** Get the approximate number of instances of a key added to this bag within a given time window. */
  def count(key: A, window: FiniteDuration): Int = bags.filter(valid(_, window)).foldMap(_.count(key))

  /** Get or create the active head bucket. */
  private[this] def head: BagBucket[A] = bags.headOption.filter(valid(_, active)) getOrElse {
    new BagBucket[A](ts.time) <| { bucket =>
      bags = bucket :: bags.filter(valid(_, expiration))
    }
  }

  /** Is a bucket valid within a time window. */
  private[this] def valid(bucket: BagBucket[A], window: FiniteDuration): Boolean =
    ts.time - bucket.created < window.toMillis

  /** For testing. */
  private[data] def bucketCount = bags.size
}

object BucketGenerationalBag {
  /**
    * Create an empty bag.
    *
    * @tparam A the element type
    * @param expiration the duration for which to retain items
    * @param buckets the number of buckets
    * @param ts the time source
    * @return the nnew empty bag
    */
  def empty[A](expiration: FiniteDuration, buckets: Int)(implicit ts: TimeSource): BucketGenerationalBag[A] =
    new BucketGenerationalBag[A](expiration, buckets, ts)

  /** A bucket within the bag. */
  private class BagBucket[A](val created: Long) {
    private[this] val counts = mutable.Map.empty[A, Int].withDefaultZero

    def count(key: A): Int = counts(key)

    def add(key: A): Unit = counts.put(key, 1 + counts(key))

  }

}
