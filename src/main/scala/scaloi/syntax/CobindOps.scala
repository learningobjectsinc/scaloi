package scaloi.syntax

import scalaz.Cobind

/**
  * Enhancements on cobinds.
  *
  * @param self the cobind
  * @tparam F the cobind type
  * @tparam A the cobound type
  */
final class CobindOps[F[_], A](val self: F[A]) extends AnyVal {
  /** Apply [self] to a side-effecting function, if applicable, discarding any result.
    *
    * @param f the side-effecting function
    * @param F cobind evidence
    * @tparam B the discarded result type
    */
  @inline
  final def coflatForeach[B](f: F[A] => B)(implicit F: Cobind[F]): Unit =
    F.cobind(self)(f)
}

/**
  * Cobind operations companion.
  */
object CobindOps extends ToCobindOps

/**
  * Implicit conversion for cobind operations.
  */
trait ToCobindOps {
  import language.implicitConversions

  /**
    * Implicit conversion from cobind to the cobind enhancements.
    * @param f the cobind
    * @tparam F the cobind type
    * @tparam A the cobound type
    */
  @inline
  implicit final def toCobindOps[F[_] : Cobind, A](f: F[A]): CobindOps[F, A] = new CobindOps(f)
}
