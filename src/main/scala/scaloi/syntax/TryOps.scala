package scaloi
package syntax

import scala.util.{Failure, Success, Try}

final class TryOps[T](private val self: Try[T]) extends AnyVal {

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

}

object TryOps extends ToTryOps

trait ToTryOps {
  import language.implicitConversions

  @inline implicit final def ToTryOps[T](t: Try[T]): TryOps[T] = new TryOps(t)
}
