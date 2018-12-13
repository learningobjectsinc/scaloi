/* package _root_ */

import scalaz._

package object scaloi {
  type Attempt[A] = Throwable \/ A

  /** A (Scala) partial function.
    *
    * Basically a [[Kleisli]] arrow over [[Option]], but using exceptions.
    */
  type =∂>[-A, +R] = PartialFunction[A, R]

  /**
    * A [[scaloi.ClassMap]] with no lower bound.
    * @tparam U the upper bound of the types of values in this [[scaloi.ClassMap]]
    */
  type ClassMap0[U] = ClassMap[U, Nothing]

  import MultiMap._
  /** For a fixed `K` and `V`, `MultiMap[K, V]` is a monoid. */
  implicit def MultiMapMonoid[K, V]: Monoid[MultiMap[K, V]] =
    new Monoid[MultiMap[K, V]] {
      def zero: MultiMap[K, V] = Map.empty

      def append(f1: MultiMap[K, V], f2: => MultiMap[K, V]) = f1 combine f2
    }
}
