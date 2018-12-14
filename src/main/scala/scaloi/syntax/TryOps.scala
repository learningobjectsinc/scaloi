package scaloi
package syntax

import scalaz.\/

import scala.util.{Failure, Success, Try}

final class TryOps[T](private val self: Try[T]) extends AnyVal {
  import \/.{left, right}

  /** Transform matching failures with the provided partial function.
    *
    * @param fn the partial function with which to transform exceptions
    * @return `self` if successful, otherwise [[Failure]] of the error,
    *         transformed by `fn` if possible.
    */
  def mapExceptions(fn: PartialFunction[Throwable, Throwable]): Try[T] =
    self match {
      case s@Success(_) => s
      case Failure(err) => Failure(fn.applyOrElse(err, (_: Throwable) => err)) // out, accursed gremlins of variance!
    }

  def disjoin[E](handler: Throwable => E): E \/ T =
    self.fold(left compose handler, right)

  /**
    * Do `fn` if this `Try` is a failure. Like `.foreach` but for failures and
    * returns the try afterwards
    *
    * @param fn side effect for throwable
    * @return this `Try`
    */
  def tapFailure(fn: Throwable => Unit): Try[T] = self match {
    case Success(_) => self
    case Failure(t) => fn(t); self
  }

}

/** Enhancements on the `Try` companion module.
  */
final class TryCompanionOps(private val self: Try.type) extends AnyVal {

  /** Constructs a `Success` of the provided value.
    *
    * The return type is widened to `Try` to help the type inferencer.
    */
  def success[A](a: A): Try[A] = Success(a)

  /** Constructs a `Failure` with the provided exception.
    *
    * The return type is widened to `Try` to help the type inferencer.
    */
  def failure[A](err: Throwable): Try[A] = Failure(err)

}

/** `TryOps` companion */
object TryOps extends ToTryOps with ToTryCompanionOps

/** Implicit conversions from `Try`s to their ops. */
trait ToTryOps {
  import language.implicitConversions

  @inline implicit final def ToTryOps[T](t: Try[T]): TryOps[T] = new TryOps(t)
}

/** Implicit conversions from the `Try` companion module to its ops. */
trait ToTryCompanionOps {
  import language.implicitConversions

  /** Implicitly convert from the `Try` companion module to its ops. */
  @inline implicit final def ToTryCompanionOps(self: Try.type): TryCompanionOps =
    new TryCompanionOps(self)

}
