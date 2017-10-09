package scaloiz.syntax

import scala.collection.mutable
import scala.language.implicitConversions
import scalaz.{Monoid, Semigroup}

/**
  * Enhancements on mutable maps.
  * @param self the mutable map
  * @tparam A the key type
  * @tparam B the value type
  */
final class MutableMapOps[A, B](val self: mutable.Map[A, B]) extends AnyVal {
  /**
    * Return a mutable map with a default value of the monoidal zero.
    * @param ev monoid evidence for the value type
    * @return the mutable map
    */
  @inline final def withDefaultZero(implicit ev: Monoid[B]): mutable.Map[A, B] = self.withDefaultValue(ev.zero)

  /**
    * Append a value to this map. If there is an existing value under the chosen key then
    * it is appended with the new value, else it is stored directly.
    * @param a the key
    * @param b the value
    * @param ev semigroup evidence for the value type
    */
  @inline final def append(a: A, b: B)(implicit ev: Semigroup[B]): Unit =
    self.update(a, self.get(a).fold(b)(ev.append(_, b)))

}

/**
  * MutableMap operations companion.
  */
object MutableMapOps extends ToMutableMapOps

/**
  * Implicit conversion for mutable map operations.
  */
trait ToMutableMapOps {

  /**
    * Implicit conversion from mutable bap to the mutable map enhancements.
    * @param m the mutable map
    * @tparam A the key type
    * @tparam B the value type
    */
  implicit def toMutableMapOps[A, B](m: mutable.Map[A, B]): MutableMapOps[A, B] =
    new MutableMapOps(m)
}
