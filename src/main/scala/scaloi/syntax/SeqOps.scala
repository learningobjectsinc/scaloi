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
