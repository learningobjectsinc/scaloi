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
  * Implicit conversion for Validation operations.
  */
trait ToValidationOps extends Any {

  implicit def toValidationNelOps[E, X](v: Validation[E, X]): ValidationOps[E, X] = new ValidationOps(v)
}
