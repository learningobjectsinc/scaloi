package scaloi.misc

import scala.collection.mutable

/**
  * Somewhat fair queue of key value pairs. Values are released from the queue fairly
  * across the key space. For example, if key 'A' enqueues 3 items, key 'B' enqueues
  * 2 items and key 'C' enqueues one item, the resulting order will be 'ABCABA'.
  *
  * @tparam A the key type
  * @tparam B the value type
  */
class UnboundedBlockingFairKeyedQueue[A, B] {
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
    * Take the next value from this queue, blocking until one becomes available.
    * @return the next value
    */
  def take(): B = takeTuple()._2

  /**
    * Take the next tuple from this queue, blocking until one becomes available.
    * @return the next tuple
    */
  def takeTuple(): (A, B) = synchronized {
    // Grab the next key and value
    val (key, value) = next()
    // If there is a value in the key queue then put it at the end of the run queue
    keyQueues.get(key) foreach { keyQueue =>
      runQueue.enqueue(key -> keyQueue.dequeue())
      if (keyQueue.isEmpty)
        keyQueues.remove(key)
    }
    key -> value
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

  /**
    * Wait for the run queue to be non-empty and then remove the first value.
    * @return the first value
    */
  private[this] def next(): (A, B) = {
    while (runQueue.isEmpty) {
      wait()
    }
    runQueue.dequeue()
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
