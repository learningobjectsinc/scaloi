package scaloi
package syntax

import scaloi.misc.Hypermonad

import scala.language.implicitConversions

/**
  * Hypermonadic syntax.
  *
  * @param fa the hypermonad value
  * @tparam A the wrapped type
  */
final class HypermonadOps[F[_], A](val fa: F[A]) extends AnyVal {
  def flatterMap[G[_], H[_], B](f: A => G[H[B]])(implicit hyper: Hypermonad[F, G, H]): F[B] =
    hyper.flatterMap(fa, f)
}

/**
  * Hypermonadic operations companion.
  */
object HypermonadOps extends ToHypermonadOps

/**
  * Implicit conversion for hypermonadic operations.
  */
trait ToHypermonadOps {

  /** Implicit conversion to hypermonadic syntax. */
  implicit def toHypermonadOps[F[A], A](fa: F[A]): HypermonadOps[F, A] = new HypermonadOps(fa)
}
