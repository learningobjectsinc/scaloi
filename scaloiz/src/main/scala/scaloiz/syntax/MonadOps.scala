package scaloiz.syntax

import scalaz.{Monad, Unapply}

/** Enhancements upon Monads. */
final class MonadOps[M[_], A](val self: M[A]) extends AnyVal {

  /**
    * Compute an effect from the contained value, then apply
    * that effect to `self`, discarding the computed value.
    *
    * @param f the effectful function
    * @param M evidence of the monadicity of `M`
    * @return `self` with the effects of `f(self)` applied
    */
  def flatTap[B](f: A => M[B])(implicit M: Monad[M]): M[A] =
    M.bind(self)(a => M.map(f(a))(_ => a))

}

object MonadOps extends ToMonadOps with ToFunctorOps

trait ToMonadOps extends ToMonadOps0 {
  import language.implicitConversions

  @inline
  implicit final def ToMonadOps[M[_] : Monad, A](self: M[A]): MonadOps[M, A] =
    new MonadOps[M, A](self)
}

trait ToMonadOps0 {
  import language.implicitConversions

  @inline
  implicit final def ToMonadOps0[MA](self: MA)(implicit UA: Unapply[Monad, MA]): MonadOps[UA.M, UA.A] =
    new MonadOps[UA.M, UA.A](UA(self))

}
