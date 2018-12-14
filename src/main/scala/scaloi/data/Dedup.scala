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

package scaloi
package data

import scaloi.misc.TimeSource

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

/**
  * A set-like container for a person of few needs, primarily deduplication.
  */
trait Dedup[A] {

  /** Add an element to this container. */
  def +=(x: A): Unit

  /** Add a sequence of elements to this container. */
  def ++=(xs: Iterable[A]): Unit

  /** Test whether an element is present in this container. */
  def contains(a: A): Boolean
}

/**
  * This class keeps a record of recent elements to allow for tasks such as efficient deduplication of events.
  *
  * This class is threadsafe.
  *
  * @param expiration the minimum length of time that elements should be retained
  * @param ts a source for time
  */
class BucketGenerationalDedup[A](expiration: FiniteDuration, buckets: Int, ts: TimeSource) extends Dedup[A] {
  import BucketGenerationalDedup._

  /** The buckets. */
  private[this] var dedups = List[TimeBucket[A]]()

  private[this] val timeout = expiration.toMillis

  /** Add an element to this container. */
  override def +=(x: A): Unit = this ++= Seq(x)

  /** Add a sequence of elements to this container. */
  override def ++=(xs: Iterable[A]): Unit = synchronized {
    val now = ts.time
    if (dedups.headOption.forall(b => b.expires <= now)) {
      dedups = new TimeBucket[A](now + timeout / buckets) :: dedups.filter(b => b.expires > now - timeout)
    }
    dedups.head ++= xs // no need to remove from old buckets
    ()
  }

  /** Test whether an element is present in this container. */
  override def contains(a: A): Boolean = synchronized {
    val oldest = ts.time - timeout
    dedups exists { b =>
      (b.expires >= oldest) && b.contains(a)
    }
  }

  /** For testing. */
  private[data] def bucketCount = dedups.size

}

object BucketGenerationalDedup {
  /**
    * Create an empty dedup.
    *
    * @tparam A the element type
    * @param expiration the duration for which to retain items
    * @param buckets the number of buckets
    * @param ts the time source
    * @return the new empty dedup
    */
  def empty[A](expiration: FiniteDuration, buckets: Int)(implicit ts: TimeSource): BucketGenerationalDedup[A] =
    new BucketGenerationalDedup[A](expiration, buckets, ts)

  /** A time bucket with its expiration time (when it should no longer accept elements). */
  private class TimeBucket[B](val expires: Long) extends mutable.HashSet[B]

}
