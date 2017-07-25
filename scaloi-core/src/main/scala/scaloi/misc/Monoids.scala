package scaloi.misc

import scalaz.{Monoid, \/, \/-}

/**
  * Miscellaneous monoids.
  */
object Monoids {
  /**
    * Monoid instance for disjunctions that fails fast on the first left. Contrast
    * with the standard DisjunctionMonoid which requires a Semigroup left type that
    * accumulates results.
    *
    * @tparam A the left type
    * @tparam B the right type
    * @return a fail-fast monoid instance
    */
  implicit def failFastDisjunctionMonoid[A, B: Monoid] = new Monoid[A \/ B] {
    override def zero: A \/ B = \/-(Monoid[B].zero)

    override def append(l: A \/ B, r: => A \/ B): A \/ B =
      for { lv <- l; rv <- r } yield {
        Monoid[B].append(lv, rv)
      }
  }

}
