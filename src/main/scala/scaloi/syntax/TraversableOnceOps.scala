package scaloi
package syntax

import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Enhancements on traversable once things.
  * @param self the traversable thing
  * @tparam F the traversable type
  * @tparam A the traversed type
  */
final class TraversableOnceOps[F[X] <: TraversableOnce[X], A](val self: F[A]) extends AnyVal {
  /**
    * Apply a map to an optional value to the elements of this traversable, returning
    * the first defined result.
    *
    * @param f the map function
    * @tparam B the target type
    * @return the optional value
    */
  @inline final def findMap[B](f: A => Option[B]): Option[B] = {
    val i = self.toIterator
    @tailrec def loop: Option[B] =
      if (i.hasNext) {
        val ob = f(i.next())
        if (ob.isDefined) {
          ob
        } else {
          loop
        }
      } else {
        None
      }
    loop
  }
}

/**
  * Standard library traversable operations companion.
  */
object TraversableOnceOps extends ToTraversableOnceOps

/**
  * Implicit conversion for standard library traversable once operations.
  */
trait ToTraversableOnceOps {

  /**
    * Implicit conversion from traversable once to its enhancements.
    * @param f: the traversable thing
    * @tparam F the traversable type
    * @tparam A the traversed type
    */
  implicit def toTraversableOnceOps[F[X] <: TraversableOnce[X], A](f: F[A]): TraversableOnceOps[F, A] =
    new TraversableOnceOps(f)
}
