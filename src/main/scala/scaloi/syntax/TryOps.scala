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

package scaloi
package syntax

import scalaz.\/
import scalaz.concurrent.Task

import scala.util.{Failure, Success, Try}
import scalaz.\/
import scalaz.syntax.either._

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

  /** Transform this to a disjunction, applying a transformation to the failure exception.
    *
    * @param f the exception transformation
    * @tparam E the resulting left type
    * @return a disjunction
    */
  def disjoin[E](f: Throwable => E): E \/ T =
    self.fold(left compose f, right)

  /**
    * Transform this to a list which is empty in the failure case
    * @return a [[List]]
    */
  def toList: List[T] =
    self match {
      case Success(t) => List(t)
      case Failure(_) => List.empty
    }

  /** An alias for [[disjoin]]. */
  def toRightDisjunction[A](f: Throwable => A): A \/ T =
    disjoin(f)

  /**
    * Like [[disjoin]] when you want the throwable on the left untransformed
    * @return a disjunction
    */
  def disjunction: Throwable \/ T = self.fold(left, right)

  /**
    * Convert this [[Try]] to an immediate [[Task]].
    * @return this try as a [[Task]].
    */
  def toTask: Task[T] = self.fold(Task.fail, Task.now)

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

  /** Transform this to a disjunction, applying a transformation to the failure exception.
    *
    * @param f the exception transformation
    * @tparam A the resulting left type
    * @return a disjunction
    */
  def \/>[A](f: Throwable => A): A \/ T =
    self match {
      case Success(s) => s.right
      case Failure(e) => f(e).left
    }

  /** Transform this to a disjunction, discarding the exception.
    *
    * @param a the left value
    * @tparam A the left type
    * @return a success as a right, else the supplied left
    */
  def \/>|[A](a: => A): A \/ T =
    self.fold(_ => left(a), right)

  /** Replaces the failure exception, if present, with another, initiaizing
    * the cause of the new exception with the original.
    *
    * Surprising Side Effect: ^^
    *
    * @param t the new failure
    * @return the resulting trf
    */
  def |<@~*(t: => Throwable): Try[T] = self match {
    case Success(_) => self
    case Failure(f) => Failure(t.initCause(f))
  }

  /** Map, semipartially, over both sides of the [[Try]].
    *
    * @param onError a partial function to map exceptions
    * @param onSuccess a function to map success
    * @tparam U the result type
    * @return the resulting [[Try]].
    */
  def bimapf[U](onError: PartialFunction[Throwable, Throwable], onSuccess: T => U): Try[U] =
    mapExceptions(onError).map(onSuccess)

  def orThrow : T = self.get
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

/** Enhancements on anything to transform to a try.
  */
final class TryAnyOps[A](private val self: A) extends AnyVal {

  /** Constructs a [[Success]] of the provided value.
    *
    * The return type is widened to [[Try]] to help the type inferencer.
    */
  @inline
  def success: Try[A] = Success(self)

  /** Constructs a [[Failure]] with the provided exception.
    *
    * The return type is widened to [[Try]] to help the type inferencer.
    */
  @inline
  def failure(implicit ev: A <:< Throwable): Try[Nothing] = Failure(ev(self))

}

/** [[TryOps]] companion. */
object TryOps extends ToTryOps with ToTryCompanionOps with ToTryAnyOps

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

/** Implicit conversions from anything to its try ops. */
trait ToTryAnyOps {
  import language.implicitConversions

  /** Implicitly convert from anything to its try ops. */
  @inline implicit final def ToTryAnyOps[A](self: A): TryAnyOps[A] =
    new TryAnyOps(self)

}
