/*
 * Copyright 2007 Learning Objects
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

import scalaz.{Validation, ValidationNel}

import scala.language.implicitConversions
import scala.util.Try


final class ValidationOps[E, X](private val self: Validation[E, X]) extends AnyVal {

  @inline def toTry(f: E => Throwable): Try[X] =
    self match {
      case scalaz.Failure(e) => scala.util.Failure(f(e))
      case scalaz.Success(x) => scala.util.Success(x)
    }
}

final class ValidationAnyOps[A](private val self: A) extends AnyVal {
  import boolean._

  /**
    * Return this as a validation success if a predicate passes, else a supplied validation failure.
    * @param f the predicate
    * @param e the failure
    * @tparam E the failure type
    * @return the resulting [[scalaz.Validation]]
    */
  def validWhen[E](f: A => Boolean, e: => E): Validation[E, A] = f(self).elseInvalid(e, self)

  /**
    * Return a supplied validation failure if a predicate passes, else this as a validation success.
    * @param f the predicate
    * @param e the failure
    * @tparam E the failure type
    * @return the resulting [[scalaz.Validation]]
    */
  def validUnless[E](f: A => Boolean, e: => E): Validation[E, A] = f(self).thenInvalid(e, self)

  /**
    * Return this as a validation success if a predicate passes, else a supplied validation failure
    * in a non-empty list.
    * @param f the predicate
    * @param e the failure
    * @tparam E the failure type
    * @return the resulting [[scalaz.ValidationNel]]
    */
  def validNelWhen[E](f: A => Boolean, e: => E): ValidationNel[E, A] = f(self).elseInvalidNel(e, self)

  /**
    * Return a supplied validation failure in a non-empty list if a predicate passes, else
    * this as a validation success.
    * @param f the predicate
    * @param e the failure
    * @tparam E the failure type
    * @return the resulting [[scalaz.ValidationNel]]
    */
  def validNelUnless[E](f: A => Boolean, e: => E): ValidationNel[E, A] = f(self).thenInvalidNel(e, self)
}

/**
  * Implicit conversion for Validation operations.
  */
trait ToValidationOps extends Any {

  implicit def toValidationNelOps[E, X](v: Validation[E, X]): ValidationOps[E, X] = new ValidationOps(v)
  implicit def toValidationAnyOps[A](a: A): ValidationAnyOps[A] = new ValidationAnyOps(a)
}
