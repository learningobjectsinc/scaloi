package scaloiz
package misc

import scalaz.{Order, Semigroup}

/**
  * Miscellaneous semigroups. Scalaz defines similar semigroups but only for tagged types.
  */
object Semigroups {

  /** The max semigroup for an untagged ordered type.
    *
    * @tparam A the type with order evidence
    * @return semigroup evidence
    */
  implicit def maxSemigroup[A : Order]: Semigroup[A] = Semigroup.instance((a, b) => Order[A].max(a, b))


  /** The min semigroup for an untagged ordered type.
    *
    * @tparam A the type with order evidence
    * @return semigroup evidence
    */
  implicit def minSemigroup[A : Order]: Semigroup[A] = Semigroup.instance((a, b) => Order[A].min(a, b))


  /**
    * Semigroup evidence for throwables, prefers the first of a series of exceptions.
    */
  implicit def throwableSemiGroup[A <: Throwable] = Semigroup.firstSemigroup[A]
}
