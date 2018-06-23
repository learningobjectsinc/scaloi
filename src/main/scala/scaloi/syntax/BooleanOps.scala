package scaloi.syntax

import java.{lang => jl}

import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.std.{BooleanOps => BooleanOpsZ}
import scalaz.{Monoid, \/}

import scala.language.implicitConversions
import scala.util.{Failure, Try}

/**
  * Enhancements on booleans.
  * @param self the boolean value
  */
final class BooleanOps(val self: Boolean) extends AnyVal {

  /**
    * Returns an option value if true, else none.
    * @param f a function that produces the optional value
    * @tparam A the value type
    * @return the optional value
    */
  def flatOption[A](f: => Option[A]): Option[A] = self.fold(f, None)

  /**
    * Returns an option value if true and nonempty, otherwise monoidal zero.
    *
    * @param f a function that produces the optional value
    * @tparam A the value type
    * @return the monoidal value
    */
  def ??? [A](f: => Option[A])(implicit A: Monoid[A]): A = self.fold(f.orZero, A.zero)

  /**
    * Run a side-effecting function if true.
    * @param f the side-effecting function
    * @tparam A the return type
    * @return the original boolean value
    */
  def <|?[A](f: => A): Boolean = {
    if (self) f
    self
  }

  /**
    * Run a side-effecting function if false.
    * @param f the side-effecting function
    * @tparam A the return type
    * @return the original boolean value
    */
  def <|![A](f: => A): Boolean = {
    if (!self) f
    self
  }

  /**
    * Returns the specified value as a left if this is true, else unitary right.
    * @param f the left value
    * @tparam A the left type
    * @return the left value or unit
    */
  def thenLeft[A](f: => A): A \/ Unit = (!self).either(()).or(f)

  /** An alias for [[thenLeft]]. */
  @inline final def <\/[A](f: A): A \/ Unit = thenLeft(f)

  /**
    * Returns the specified value as a left if this is false, else unitary right.
    * @param f the left value
    * @tparam A the left type
    * @return the left value or unit
    */
  def elseLeft[A](f: => A): A \/ Unit = self.either(()).or(f)

  /** An alias for [[elseLeft]]. */
  @inline final def \/>[A](f: => A): A \/ Unit = elseLeft(f)

  /**
    * Return an optional value if this is false. The opposite of `.option`.
    * @param a the value
    * @tparam A the value type
    * @return some of the value if this is false
    */
  def noption[A](a: => A): Option[A] = (!self).option(a)

  /** Return unit success if this is true, otherwise fail with the given error.
    *
    * @param err the error with which to fail
    * @return `Success(())` if this is true, or `Failure(err)` otherwise
    */
  def elseFailure(err: => Throwable): Try[Unit] =
    if (self) successUnit else Failure(err)

  /**
    * An alias for [[elseFailure]].
    */
  @inline def <@~*(failure: => Throwable): Try[Unit] = elseFailure(failure)

  /** Return unit success if this is false, otherwise fail with the given error.
    *
    * @param err the error with which to fail
    * @return `Success(())` if this is false, or `Failure(err)` otherwise
    */
  def thenFailure(err: => Throwable): Try[Unit] =
    if (self) Failure(err) else successUnit
}

/**
  * Enhancements on boolean conditional eithers.
  *
  * @param self the conditional either
  * @tparam A the result type
  */
final class BooleanConditionalEitherOps[A](val self: BooleanOpsZ#ConditionalEither[A]) extends AnyVal {

  /**
    * Returns the positive result of the conditional, if true, or else a supplied disjunction
    * value.
    *
    * For example:
    * ```
    * true either "Happy" orElse "Sad".right === Happy.right
    * false either "Happy" orElse "Sad".right === Sad.right
    * false either "Happy" orElse "Sad".left === Sad.left
    * ```
    *
    * @param d the disjunction value if the conditional is false
    * @tparam B the left type
    * @tparam C the right type
    * @return the resulting disjunction
    */
  def orElse[B, C >: A](d: => B \/ C): B \/ C = self.or(()).orElse(d)
}

/**
  * Boolean operations companion.
  */
object BooleanOps extends ToBooleanOps

/**
  * Implicit conversion for boolean operations.
  */
trait ToBooleanOps {

  /**
    * Implicit conversion from a boolean to enhancements.
    * @param value the boolean
    */
  implicit def toBooleanOps(value: Boolean): BooleanOps = new BooleanOps(value)

  /**
    * Implicit conversion from a boxed boolean to enhancements.
    * @param value the boolean
    */
  implicit def toBooleanOps(value: jl.Boolean): BooleanOps = new BooleanOps(value.booleanValue)

  /**
    * Implicit conversion from a boxed boolean to scalaz enhancements.
    * @param value the boolean
    */
  implicit def toBooleanOpz(value: jl.Boolean): BooleanOpsZ = new BooleanOpsZ(value.booleanValue)

  /**
    * Implicit conversion from boolean conditional either to the enhancements.
    * @param e the conditional either
    * @tparam A its type
    */
  implicit def toBooleanConditionalEither[A](e: BooleanOpsZ#ConditionalEither[A]): BooleanConditionalEitherOps[A] =
    new BooleanConditionalEitherOps(e)
}
