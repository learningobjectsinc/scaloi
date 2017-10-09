import scaloi.MultiMap.MultiMap

import scalaz.{Monoid, \/}
import scaloi.MultiMap._

/* package _root_ */

package object scaloiz {
  type Attempt[A] = Throwable \/ A

  /** For a fixed `K` and `V`, `MultiMap[K, V]` is a monoid. */
  implicit def MultiMapMonoid[K, V]: Monoid[MultiMap[K, V]] =
    new Monoid[MultiMap[K, V]] {
      def zero: MultiMap[K, V] = Map.empty

      def append(f1: MultiMap[K, V], f2: => MultiMap[K, V]) = f1 combine f2
    }
}
