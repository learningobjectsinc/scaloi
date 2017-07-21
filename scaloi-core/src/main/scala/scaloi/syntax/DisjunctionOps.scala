package scaloi.syntax

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scalaz.\/
import scalaz.syntax.monad._

/**
  * Enhancements on disjunctions.
  *
  * @param self the disjunction
  * @tparam A the left type
  * @tparam B the right type
  */
final class DisjunctionOps[A, B](val self: A \/ B) extends AnyVal {
  import AnyOps._

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
    * Get the right value or throw the throwable from the left value.
    *
    * @param ev evidence for the throwable of the left type
    * @return the right value
    */
  def orThrow(implicit ev: A <:< Throwable): B = self.valueOr(a => throw ev(a))

  /**
    * Convert this to a `Try` if the left has a throwable.
    *
    * @param ev evidence for the throwable of the left type
    * @return the resulting `Try`
    */
  def toTry(implicit ev: A <:< Throwable): Try[B] = self.fold(e => Failure(ev(e)), Success.apply)

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
