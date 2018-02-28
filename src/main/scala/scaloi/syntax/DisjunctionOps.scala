package scaloi.syntax

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scalaz.\/
import scaloi.syntax.AnyOps._
import scalaz.syntax.bind._

/**
  * Enhancements on disjunctions.
  *
  * @param self the disjunction
  * @tparam A the left type
  * @tparam B the right type
  */
final class DisjunctionOps[A, B](val self: A \/ B) extends AnyVal {

  /**
    * Left tap: apply a kestrel combinator to the left value of a disjunction.
    * Runs a side-effect on the left value and returns this disjunction unchanged.
    *
    * @param f the function to apply to the left value
    * @tparam T the result type of the function
    * @return the left value
    */
  def leftTap[T](f: A => T): A \/ B = self leftMap {
    _ <| f
  }

  /**
    * An alias for leftTap.
    */
  @inline final def -<|[T](f: A => T): A \/ B = leftTap(f)

  /**
    * Right tap: apply a kestrel combinator to the right value of a disjunction.
    * Runs a side-effect on the right value and returns this disjunction unchanged.
    *
    * @param f the function to apply to the right value
    * @tparam T the result type of the function
    * @return the right value
    */
  def rightTap[T](f: B => T): A \/ B = self map {
    _ <| f
  }

  /**
    * An alias for rightTap.
    */
  @inline final def <|-[T](f: B => T): A \/ B = rightTap(f)

  /**
    * An alias for leftMap.
    */
  @inline final def -\/>[C](f: A => C): C \/ B = self leftMap f

  /**
    * Return this disjunction if a left, otherwise the supplied disjunction.
    * @param d the supplied disjunction
    * @tparam AA the left type
    * @tparam C the right type
    * @return this, if left, or else that
    */
  def andThen[AA >: A, C](d: => AA \/ C): AA \/ C = self.flatMap(_ => d)

  /**
    * Get the right value or throw the throwable from the left value.
    *
    * @param ev evidence for the throwable of the left type
    * @return the right value
    */
  def orThrow(implicit ev: A <:< Throwable): B = self.valueOr(a => throw ev(a))

  /**
    * Convert this to a `Try` if the left is a throwable.
    *
    * @param ev evidence for the throwable of the left type
    * @return the resulting `Try`
    */
  def toTry(implicit ev: A <:< Throwable): Try[B] = self.fold(e => Failure(ev(e)), Success.apply)

  /**
    * Convert this to a `Try`, explicitly converting the left to an exception.
    *
    * @param ex function to turn the left into an exception
    * @return the resulting `Try`
    */
  def toTry(ex: A => Throwable): Try[B] = self.fold(e => Failure(ex(e)), Success.apply)

  /**
    * Convert this to a `Future` if the left is a throwable.
    *
    * @param ev evidence for the throwable of the left type
    * @return the resulting `Try`
    */
  def toFuture(implicit ev: A <:< Throwable): Future[B] = self.fold(e => Future.failed(ev(e)), Future.successful)

  /**
    * Convert this to a `Future`, explicitly converting the left to an exception.
    *
    * @param ex function to turn the left into an exception
    * @return the resulting `Try`
    */
  def toFuture(ex: A => Throwable): Future[B] = self.fold(e => Future.failed(ex(e)), Future.successful)

  /**
    * Evaluate `f` on the right, catching errors, and join everything together.
    *
    * @param f the function to apply to the right value
    * @return the result of `f` flat-mapped into self, or an error on the left.
    */
  def tryFlatMap[AA >: A, T](f: B => AA \/ T)(implicit liskaa: AA <:< Throwable, liska: A <:< Throwable): Throwable \/ T =
    self.leftMap(liska).flatMap(b => \/.fromTryCatchNonFatal(f(b) leftMap liskaa).join)
}

/**
  * Disjunction operations companion.
  */
object DisjunctionOps extends ToDisjunctionOps {

  /**
    * Try to evaluate a function, returning either the result or any non-fatal
    * error that was thrown. Pronounced try-ther.
    * @param f a function producing a value
    * @tparam T the function result type
    * @return either a throwable or the function result
    */
  def treither[T](f: => T): Throwable \/ T = \/.fromTryCatchNonFatal(f)

  /**
    * An alias for treither.
    */
  @inline final def \@~*/[T](f: => T): Throwable \/ T = treither(f)

}

/**
  * Implicit conversion for disjunction operations.
  */
trait ToDisjunctionOps {

  /**
    * Implicit conversion from disjunction to the disjunction enhancements.
    * @param d the disjunction value
    * @tparam A the left type
    * @tparam B the right type
    */
  implicit def toDisjunctionOps[A, B](d: A \/ B): DisjunctionOps[A, B] = new DisjunctionOps(d)
}
