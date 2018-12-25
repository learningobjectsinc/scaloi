/*
 * Copyright 2007 Cengage Learning, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scaloi.syntax

import java.{lang => jl}

import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.std.{BooleanOps => BooleanOpsZ}
import scalaz.{Monoid, Validation, ValidationNel, \/}
import scalaz.syntax.validation._

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Enhancements on booleans.
  * @param self the boolean value
  */
final class BooleanOps(private val self: Boolean) extends AnyVal {

  /**
    * Returns an option value if true, else none.
    * @param f a function that produces the optional value
    * @tparam A the value type
    * @return the optional value
    */
  def flatOption[A](f: => Option[A]): Option[A] = if (self) f else None

  /**
    * Returns an option value if false, else none.
    * @param f a function that produces the optional value
    * @tparam A the value type
    * @return the optional value
    */
  def flatNoption[A](f: => Option[A]): Option[A] = if (self) None else f

  /** An alias for [[flatOption]]. */
  def ?-?[A](f: => Option[A]): Option[A] = flatOption(f): @inline

  /**
    * Returns an option value if true and nonempty, otherwise monoidal zero.
    *
    * @param f a function that produces the optional value
    * @tparam A the value type
    * @return the monoidal value
    */
  def ???[A](f: => Option[A])(implicit A: Monoid[A]): A = self.fold(f.orZero, A.zero)

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
    * Returns the specified value as a left if this is true, else false right.
    * @param f the left value
    * @tparam A the left type
    * @return the left value or false
    */
  def thenLeft[A](f: => A): A \/ Boolean = (!self).either(false).or(f)

  /** An alias for [[thenLeft]]. */
  @inline def \/>![A](f: A): A \/ Boolean = thenLeft(f)

  /**
    * Returns the specified value as a left if this is false, else true right.
    * @param f the left value
    * @tparam A the left type
    * @return the left value or unit
    */
  def elseLeft[A](f: => A): A \/ Boolean = self.either(true).or(f)

  /** An alias for [[elseLeft]]. */
  @inline def \/>[A](f: => A): A \/ Boolean = elseLeft(f)

  /**
    * Return an optional value if this is false. The opposite of `.option`.
    * @param a the value
    * @tparam A the value type
    * @return some of the value if this is false
    */
  def noption[A](a: => A): Option[A] = (!self).option(a)

  /** Return true success if this is true, otherwise fail with the given error.
    *
    * @param err the error with which to fail
    * @return `Success(true)` if this is true, or `Failure(err)` otherwise
    */
  def elseFailure(err: => Throwable): Try[Boolean] =
    if (self) Success(true) else Failure(err)

  /**
    * An alias for [[elseFailure]].
    */
  @inline def <@~*(err: => Throwable): Try[Boolean] = elseFailure(err)

  /** Return false success if this is false, otherwise fail with the given error.
    *
    * @param err the error with which to fail
    * @return `Success(false)` if this is false, or `Failure(err)` otherwise
    */
  def thenFailure(err: => Throwable): Try[Boolean] =
    if (self) Failure(err) else Success(false)

  /**
    * An alias for [[thenFailure]].
    */
  @inline def *~@>(err: => Throwable): Try[Boolean] = thenFailure(err)

  /**
    * scalaz.Validation version
    *
    * If self == true, return Validation.success[X, A](that)
    * else return Validation.failure[X, A](err)
    * @param err the error function
    * @param that the success value
    * @tparam E the error return type
    * @tparam A the success type
    * @return Validation.success[X, A](that) if true, Validation.failure[X, A](err) if false
    */
  def elseFailure[E, A](err: => E, that: A): Validation[E, A] =
    if (self) that.success[E] else err.failure[A]

  /**
    * scalaz.Validation version
    *
    * If self == true, return Validation.failure[X, A](err)
    * else return Validation.success[X, A](that)
    * @param err the error function
    * @param that the success value
    * @tparam E the error return type
    * @tparam A the success type
    * @return Validation.failure[X, A](err) if true, Validation.success[X, A](that) if false
    */
  def thenFailure[E, A](err: => E, that: A): Validation[E, A] =
    if (self) err.failure[A] else that.success[E]

  /**
    * scalaz.ValidationNel version
    *
    * If self == true, return ValidationNel.success[X, A](that)
    * else return ValidationNel.failure[X, A](err)
    * @param err the error function
    * @param that the success value
    * @tparam E the error type
    * @tparam A the success type
    * @return ValidationNel.success[X, A](that) if true, ValidationNel.failure[X, A](err) if false
    */
  def elseFailureNel[E, A](err: => E, that: A): ValidationNel[E, A] =
    if (self) that.successNel[E] else err.failureNel[A]

  /**
    * scalaz.ValidationNel version
    *
    * If self == true, return ValidationNel.failure[X, A](err)
    * else return ValidationNel.success[X, A](that)
    * @param err the error function
    * @param that the success value
    * @tparam E the error type
    * @tparam A the success type
    * @return ValidationNel.failure[X, A](err) if true, ValidationNel.success[X, A](that) if false
    */
  def thenFailureNel[E, A](err: => E, that: A): ValidationNel[E, A] =
    if (self) err.failureNel[A] else that.successNel[E]
}

/**
  * Enhancements on boolean conditional eithers.
  *
  * @param self the conditional either
  * @tparam A the result type
  */
final class BooleanConditionalEitherOps[A](private val self: BooleanOpsZ#ConditionalEither[A]) extends AnyVal {

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

  import scaloi.syntax.⋁._

  /**
    * Returns the positive result of the conditional, if true, as a success,
    * or else the supplied result as a failure.
    * @param e the failure
    * return the [[Try]].
    */
  def orFailure(e: => Throwable): Try[A] = self.or(e).toTry

  def orInvalidNel[B](e: => B): ValidationNel[B, A] = self.or(e).fold(_.failureNel, _.successNel)
}

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
