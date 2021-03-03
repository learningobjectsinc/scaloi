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

import scalaz.EphemeralStream

final class EStreamOps[A](private val self: EphemeralStream[A]) extends AnyVal {
  /**
    * Is this ephemeral stream non-empty.
    */
  def nonEmpty: Boolean = !self.isEmpty

  def foreach[U](f: A => U): Unit =
    self.foldLeft(())((_, b) => f(b))
}

trait ToEStreamOps {
  import language.implicitConversions

  @inline implicit final def ToEStreamOps[A](self: EphemeralStream[A]): EStreamOps[A] =
    new EStreamOps[A](self)
}
