package scaloi.syntax

import scalaz.Validation

import scala.language.implicitConversions
import scala.util.Try


final class ValidationOps[E, X](private val self: Validation[E, X]) extends AnyVal {

  @inline def toTry(f: E => Throwable): Try[X] =
    self match {
      case scalaz.Failure(e) => scala.util.Failure(f(e))
      case scalaz.Success(x) => scala.util.Success(x)
    }
}

/**
  * Validation operations companion.
  */
object ValidationOps extends ToValidationOps

/**
  * Implicit conversion for Validation operations.
  */
trait ToValidationOps extends Any {

  implicit def toValidationNelOps[E, X](v: Validation[E, X]): ValidationOps[E, X] = new ValidationOps(v)
}
