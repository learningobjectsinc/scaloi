package scaloi
package syntax

import scalaz.{Align, Monoid, \&/ => These}

/** Operations on [[Align]]able things.
  */
class AlignOps[F[_], A](private val fa: F[A]) extends AnyVal {
  import These.{This, That, Both}

  /** Align this and `fb`, using `a` and `b` as default values for those types.
    *
    * @param fb the other [[F]] to align this with
    * @param a  the value to pad with on the right
    * @param b  the value to pad with on the left
    * @return an aligned `F[(A, B)]`
    */
  //noinspection VariablePatternShadow (it looks nice this way)
  def zipWithDefault[B](fb: F[B])(a: => A, b: => B)(implicit F: Align[F]): F[(A, B)] =
    F.alignWith[A, B, (A, B)] { // scalaz pls
      case This(a)    => (a, b)
      case That(b)    => (a, b)
      case Both(a, b) => (a, b)
    } (fa, fb)

  /** Align this and `fb`, using monoidal defaults for `A` and `B`.
    *
    * @param fb the other [[F]] to align this with
    * @return an aligned `F[(A, B)]`
    */
  def zipM[B](fb: F[B])(implicit F: Align[F], A: Monoid[A], B: Monoid[B]): F[(A, B)] =
    zipWithDefault(fb)(A.zero, B.zero)(F)

}

/** [[AlignOps]] companion. */
object AlignOps extends ToAlignOps

/** Implicit conversions to [[AlignOps]]. */
trait ToAlignOps {
  import language.implicitConversions

  /** Implicitly convert to [[AlignOps]]. */
  // XXX: F: Align constraint removed as it baffles intellij
  @inline implicit final def ToAlignOps[F[_]/*: Align*/, A](fa: F[A]): AlignOps[F, A] =
    new AlignOps[F, A](fa)
}
