package scaloi
package syntax

import scala.language.implicitConversions

/**
  * Enhancements on seqs.
  *
  * @param self the seq
  * @tparam A the seq value type
  */
final class SeqOps[A](val self: Seq[A]) extends AnyVal {

  /**
    * Returns whether this seq has a given size.
    * @param size the size against which to test
    * @return whether this seq has that size
    */
  def hasSize(size: Int): Boolean = self.lengthCompare(size) == 0

  /**
    * Group a seq to a map of values grouped by the specified value function.
    *
    * @param keyFn    The function transforming the entries to map keys.
    * @param valueFn  The function transforming the entries to values in the map.
    * @tparam K       The key type.
    * @tparam V       The grouped value type.
    * @return         Map of values grouped by the given key function
    */
  def groupMap[K, V](keyFn: A => K, valueFn: A => V): Map[K, Seq[V]] =
    self.map(e => keyFn(e) -> valueFn(e)).groupBy(_._1).transform((_, tuples) => tuples.map(_._2))
}

/**
  * Seq operations companion.
  */
object SeqOps extends ToSeqOps

/**
  * Implicit conversion for seq tag operations.
  */
trait ToSeqOps {

  /**
    * Implicit conversion from seq to the seq enhancements.
    * @param c the seq
    * @tparam C its type
    */
  implicit def toSeqOps[C](c: Seq[C]): SeqOps[C] = new SeqOps(c)
}
