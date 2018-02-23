package scaloi
package data

import scaloi.misc.TimeSource

import scala.collection.mutable

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
  * Inspired by twitter BucketGenerationalQueue this class keeps a record of recent
  * elements to allow for tasks such as efficient deduplication of events.
  * @param timeout the minimum length of time (in milliseconds) that elements should be retained
  * @param ts a source for time
  */
class BucketGenerationalDedup[A](timeout: Long)(implicit ts: TimeSource) extends Dedup[A] {
  import BucketGenerationalDedup._

  /** The buckets. */
  private[this] var buckets = List[TimeBucket[A]]()

  /** Add an element to this container. */
  override def +=(x: A): Unit = this ++= Seq(x)

  /** Add a sequence of elements to this container. */
  override def ++=(xs: Iterable[A]): Unit = synchronized {
    val now = ts.time
    if (buckets.headOption.forall(b => b.expires <= now)) {
      buckets = new TimeBucket[A](now + timeout / Buckets) :: buckets.filter(b => b.expires > now - timeout)
    }
    buckets.head ++= xs // no need to remove from old buckets
    ()
  }

  /** Test whether an element is present in this container. */
  override def contains(a: A): Boolean = synchronized {
    val oldest = ts.time - timeout
    buckets exists { b =>
      (b.expires >= oldest) && b.contains(a)
    }
  }

  /** For testing. */
  private[data] def bucketCount = buckets.size

}

object BucketGenerationalDedup {

  /** The number of buckets. */
  final val Buckets = 3

  /** A time bucket with its expiration time (when it should no longer accept elements). */
  class TimeBucket[B](val expires: Long) extends mutable.HashSet[B]

}
