package scaloi
package syntax

import scala.language.implicitConversions

/**
  * Enhancements on sets.
  *
  * @param self the set
  * @tparam A the set value type
  */
final class SetOps[A](val self: Set[A]) extends AnyVal {

  /**
    * Convert this set to a map with a function from keys to values.
    *
    * @param f a function from keys to values
    * @tparam B the value type
    * @return the resulting map
    */
  def mapTo[B](f: A => B): Map[A, B] = self.map(a => a -> f(a)).toMap
}

/**
  * Set operations companion.
  */
object SetOps extends ToSetOps

/**
  * Implicit conversion for set tag operations.
  */
trait ToSetOps {

  /**
    * Implicit conversion from set to the set enhancements.
    * @param c the set
    * @tparam C its type
    */
  implicit def toSetOps[C](c: Set[C]): SetOps[C] = new SetOps(c)
}
