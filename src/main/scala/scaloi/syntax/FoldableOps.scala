package scaloi
package syntax

import scala.language.implicitConversions
import scalaz.Foldable

/**
  * Enhancements on foldable things.
  * @param self the foldable thing
  * @tparam F the foldable type
  * @tparam A the folded type
  */
final class FoldableOps[F[_], A](val self: F[A]) extends AnyVal {
  /**
    * Apply a map to an optional value to the elements of this foldable, returning
    * the first defined result.
    *
    * @param f the map function
    * @param ev foldable evidence
    * @tparam B the target type
    * @return the optional value
    */
  @inline final def findMap[B](f: A => Option[B])(implicit ev: Foldable[F]): Option[B] =
    ev.findMapM[scalaz.Id.Id, A, B](self)(f)
}

/**
  * Foldable operations companion.
  */
object FoldableOps extends ToFoldableOps

/**
  * Implicit conversion for foldable operations.
  */
trait ToFoldableOps {

  /**
    * Implicit conversion from foldable to its enhancements.
    * @param f: the foldable thing
    * @tparam F the foldable type
    * @tparam A the folded type
    */
  implicit def toFoldableOps[F[_] : Foldable, A](f: F[A]): FoldableOps[F, A] =
    new FoldableOps(f)
}
