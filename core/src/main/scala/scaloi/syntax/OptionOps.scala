package scaloi
package syntax

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Enhancements on options.
  * @param self the option value
  * @tparam A the option type
  */
final class OptionOps[A](val self: Option[A]) extends AnyVal {
  import AnyOps._

  /**
    * Flatmap over a function to a nullable value.
    * @param f the mapping function
    * @tparam B the result type
    * @return the resulting option
    */
  @inline def flatOpt[B >: Null](f: A => B): Option[B] =
    self.flatMap(a => Option(f(a)))

  /**
    * Convert an option into a successful future, if present, else a supplied failure.
    * @param e the exception if this option is absent
    * @return an available future
    */
  @inline def toFuture(e: => Exception): Future[A] =
    self.fold(Future.failed[A](e))(Future.successful)

  /**
    * Return this option, if it does not contain the specified value, else None.
    * @param a the value to remove
    * @return this option without the specified value
    */
  @inline def -(a: A): Option[A] = self.filter(_ != a)

  /**
    * Kestrel combinator on the value of an option.
    * @param f the side-effecting function
    * @tparam B the result type
    * @return this option
    */
  @inline def tap[B](f: A => B): Option[A] = self <| { _ foreach f }

  /**
    *  An alias for tap.
    */
  @inline def <|?[B](f: A => B): Option[A] = tap(f)

  /**
    * A successful [[scala.util.Try]] of this option if present, or the given failure if empty.
    * @param failure the [[scala.util.Try]] failure if this option is empty
    * @return this option as a [[scala.util.Try]]
    */
  def toTry(failure: => Throwable): Try[A] =
    self.fold[Try[A]](Failure(failure))(Success(_))



  /**
    * Transforms `target` with the contained function if it exists,
    * otherwise, returns `target`.
    *
    * @param target the value to be potentially transformed
    * @return target transformed by the contained function, if it exists, otherwise target.
    */
  def transforming[T](target: T)(implicit ev: A <:< (T => T)): T =
    self.fold(target)(f => ev(f)(target))

  /**
    * An alias for `transforming`.
    */
  @inline def ~?>[T, U](target: T)(implicit ev: A <:< (T => T)): T =
    this transforming target
}

/**
  * Option operations companion.
  */
object OptionOps extends ToOptionOps

/**
  * Implicit conversion for option operations.
  */
trait ToOptionOps extends Any {

  /**
    * Implicit conversion from option to the option enhancements.
    * @param o the optional thing
    * @tparam A its type
    */
  implicit def toOptionOps[A](o: Option[A]): OptionOps[A] = new OptionOps(o)
}
