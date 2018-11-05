package scaloi.syntax

import scalaz.{Equal, MonadPlus, Unapply}
import scalaz.syntax.equal._
import scaloi.Zero

/** Enhancements upon MonadPlus. */
final class MonadPlusOps[M[_], A](val self: M[A]) extends AnyVal {

  /** Filter out zero values from `self`.
    *
    * @param M [[MonadPlus]] evidence for `M`
    * @param Z [[Zero]] evidence for `A`
    * @param E [[Equal]] evidence for `A`
    * @return `self' without the zeroes`
    */
  def filterNZ(implicit M: MonadPlus[M], Z: Zero[A], E: Equal[A]): M[A] =
    M.filter(self)(_ =/= Z.zero)

}

object MonadPlusOps extends ToMonadPlusOps with ToMonadOps

trait ToMonadPlusOps extends ToMonadPlusOps0 {
  import language.implicitConversions

  @inline
  implicit final def ToMonadPlusOps[M[_]: MonadPlus, A](self: M[A]): MonadPlusOps[M, A] =
    new MonadPlusOps[M, A](self)
}

trait ToMonadPlusOps0 {
  import language.implicitConversions

  @inline
  implicit final def ToMonadPlusOps0[MA](self: MA)(implicit UA: Unapply[MonadPlus, MA]): MonadPlusOps[UA.M, UA.A] =
    new MonadPlusOps[UA.M, UA.A](UA(self))

}
