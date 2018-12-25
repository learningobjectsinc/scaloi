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

import scaloi.syntax.any._

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

/**
  * Somewhat fair queue of key value pairs. Values are released from the queue fairly
  * across the key space. For example, if key 'A' enqueues 3 items, key 'B' enqueues
  * 2 items and key 'C' enqueues one item, the resulting order will be 'ABCABA'.
  *
  * @tparam A the key type
  * @tparam B the value type
  */
final class UnboundedBlockingFairKeyedQueue[A, B] {
  /** Queue of key-value pairs to be run; only one element with a given key can be in this queue at a time. */
  private[this] val runQueue = mutable.Queue.empty[(A, B)]

  /** Backlog of values for keys already in the run queue. */
  private[this] val keyQueues = mutable.Map.empty[A, mutable.Queue[B]]

  /**
    * Offer a new key value pair to the queue.
    * @param key the key
    * @param value the value
    */
  def offer(key: A, value: B): Unit = synchronized {
    if (runQueue.exists(_._1 == key)) {
      // If a value for this key is already in the run queue, push this new value onto the key queue
      keyQueues.getOrElseUpdate(key, mutable.Queue.empty).enqueue(value)
    } else {
      // Otherwise just add his value to the run queue
      runQueue.enqueue(key -> value)
      notify()
    }
  }

  /**
    * Offer a new key value pair to the queue as a tuple.
    * @param tuple the tuple
    */
  def offer(tuple: (A, B)): Unit = offer(tuple._1, tuple._2)

  /**
    * Take the next value from this queue, blocking until one becomes available.
    * @return the next value
    */
  def take(): B = takeTuple()._2

  /**
    * Take the next value from this queue, blocking until one becomes available
    * or a specified amount of time has elapsed.
    * @param timeout the maximum amount of time to wait
    * @return the next value, or [[None]] if a value could not be taken within the timeout.
    */
  def take(timeout: FiniteDuration): Option[B] = takeTuple(timeout) map (_._2)

  /**
    * Take the next tuple from this queue, blocking until one becomes available.
    * @return the next tuple
    */
  def takeTuple(): (A, B) = synchronized {
    // Grab the next key and value
    val (key, value) = next()
    shiftKeyQueue(key)
    key -> value
  }

  /**
    * Take the next tuple from this queue, blocking until one becomes available
    * or a specified amount of time has elapsed.
    * @param timeout the maximum amount of time to wait
    * @return the next tuple, or [[None]] if a value could not be taken within the timeout.
    */
  def takeTuple(timeout: FiniteDuration): Option[(A, B)] = synchronized {
    next(timeout) tap {
      case Some((key, _)) => shiftKeyQueue(key)
      case None => Unit
    }
  }

  /**
    * Clear this queue.
    */
  def clear(): Unit = synchronized {
    runQueue.clear()
    keyQueues.clear()
  }

  /**
    * Return a map of the values in this queue.
    * @return the values as a map
    */
  def toMap: Map[A, List[B]] = synchronized {
    runQueue.foldLeft(keyQueues.toMap.mapValues(_.toList)) {
      case (map, (key, value)) => map + (key -> (value :: map.getOrElse(key, List.empty)))
    }
  }

  /**
    * Get the size of this queue.
    * @return the total number of elements in this queue
    */
  def size: Long = synchronized {
    runQueue.size.toLong + keyQueues.values.map(_.size).sum
  }

  /**
    * Test whether this is empty.
    * @return whether this is empty
    */
  def isEmpty: Boolean = synchronized {
    runQueue.isEmpty
  }

  /**
    * Test whether this is non empty.
    * @return whether this is non empty
    */
  def nonEmpty: Boolean = !isEmpty

  /* implementation details follow */

  /**
    * Wait for the run queue to be non-empty and then remove the first value.
    * @return the first value
    * @throws InterruptedException if the wait is interrupted
    */
  @throws[InterruptedException]
  private[this] def next(): (A, B) = {
    while (runQueue.isEmpty) {
      wait()
    }
    runQueue.dequeue()
  }

  /**
    * Wait for the run queue to be non-empty and then remove the first value.
    * @param timeout the maximum duration to wait for the run queue to be non-empty
    * @return the first value, or [[None]] if the queue did not become non-empty
    *         within the timeout
    * @throws InterruptedException if the wait is interrupted
    */
  @throws[InterruptedException]
  private[this] def next(timeout: FiniteDuration): Option[(A, B)] = {
    if (runQueue.isEmpty) {
      wait(timeout.toMillis)
    }

    if(runQueue.nonEmpty)
      Some(runQueue.dequeue())
    else
      None
  }

  /**
    * Move the next value from the queue for the provided key to the end of the
    * run queue, if one exists. Also removes the key queue if it becomes empty.
    * @param key the key for the queue to shift
    */
  private[this] def shiftKeyQueue(key: A): Unit = {
    keyQueues.get(key) foreach { keyQueue =>
      runQueue.enqueue(key -> keyQueue.dequeue())
      if (keyQueue.isEmpty)
        keyQueues.remove(key)
    }
  }
}

/** Queue companion. */
object UnboundedBlockingFairKeyedQueue {
  /**
    * Create an empty unbounded blocking fair keyed queue.
    * @tparam A the key type
    * @tparam B the value type
    * @return the queue
    */
  def empty[A, B]: UnboundedBlockingFairKeyedQueue[A, B] = new UnboundedBlockingFairKeyedQueue[A, B]
}
