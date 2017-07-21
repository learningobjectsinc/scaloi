package scaloi.misc

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
  def maxSemigroup[A : Order]: Semigroup[A] = Semigroup.instance((a, b) => Order[A].max(a, b))


  /** The min semigroup for an untagged ordered type.
    *
    * @tparam A the type with order evidence
    * @return semigroup evidence
    */
  def minSemigroup[A : Order]: Semigroup[A] = Semigroup.instance((a, b) => Order[A].min(a, b))
}
