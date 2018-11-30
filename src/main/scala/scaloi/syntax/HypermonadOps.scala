package scaloi
package syntax

import scalaz.Id.Id
import scaloi.misc.Hypermonad

import scala.language.implicitConversions

/**
  * Hypermonadic syntax.
  *
  * @param fa the functored value
  * @tparam A the wrapped type
  */
final class HypermonadOps[F[_], A](val fa: F[A]) extends AnyVal {
  def flatterMap[G[_], H[_], B](f: A => G[H[B]])(implicit hyper: Hypermonad[F, G, H]): F[B] =
    hyper.flatterMap(fa, f)

  def hyperFlatMap[G[_], H[_], B](f: A => G[H[B]])(implicit hyper: Hypermonad[F, G, H]): F[B] =
    hyper.flatterMap(fa, f)

  def hyperFlatMap1[G[_], B](f: A => G[B])(implicit hyper: Hypermonad[F, G, Id]): F[B] =
    hyper.flatterMap(fa, f)

  def hyperFlatten[G[_], H[_], B](implicit ev: A <:< G[H[B]], hyper: Hypermonad[F, G, H]): F[B] =
    hyper.flatterMap(fa, ev)

  def hyperFlatten1[G[_], B](implicit ev: A <:< G[B], hyper: Hypermonad[F, G, Id]): F[B] =
    hyper.flatterMap(fa, ev)
}

/**
  * Hypermonadic syntax when you know you're hyper.
  *
  * @param fgha the hypermonad value
  * @tparam A the wrapped type
  */
final class EndoHypermonadOps[F[_], G[_], H[_], A](val fgha: F[G[H[A]]]) extends AnyVal {
  def hyperFlattenE(implicit hyper: Hypermonad[F, G, H]): F[A] =
    hyper.flatterMap(fgha, (gha: G[H[A]]) => gha)
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
  implicit def toHypermonadOps[F[_], A](fa: F[A]): HypermonadOps[F, A] = new HypermonadOps(fa)

  /** Implicit conversion to endohypermonadic syntax. */
  implicit def toEndoHypermonadOps[F[_], G[_], H[_], A](fgha: F[G[H[A]]]): EndoHypermonadOps[F, G, H, A] =
    new EndoHypermonadOps[F, G, H, A](fgha)
}
