package scaloi
package syntax

import scalaz.Functor
import scala.language.implicitConversions

/**
  * Enhancements on functors.
  * @param self the functor
  * @param F the functor evidence
  * @tparam F the functor type
  * @tparam A the functed type
  */
final class FunctorOps[F[_], A](val self: F[A])(implicit val F: Functor[F]) {
  /** Apply a partial function to the values in this functor. Values for
    * which the partial function is undefined remain unchanged.
    * @param pf the partial function
    * @tparam A1 the result type
    * @return the partially transformed functor
    */
  @inline final def pfmap[A1 >: A](pf: PartialFunction[A, A1]): F[A1] =
    F.map(self)(fa => pf.applyOrElse(fa, (a: A) => a))
}

/**
  * Functor operations companion.
  */
object FunctorOps extends ToFunctorOps

/**
  * Implicit conversion for functor operations.
  */
trait ToFunctorOps {

  /**
    * Implicit conversion from functor to the functor enhancements.
    * @param f the functor
    * @tparam F the functor type
    * @tparam A the functed type
    */
  implicit def toFunctorOps[F[_] : Functor, A](f: F[A]): FunctorOps[F, A] = new FunctorOps(f)
}
