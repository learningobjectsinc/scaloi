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

import scalaz.{Equal, Monoid}

/** Enhancements on [[Monoid]](s). */
final class MonoidOps[M](private val self: M) extends AnyVal {

  /** New Zealand map.
    *
    * Applies `f` to `self` if non-empty; otherwise, passes through the empty value.
    * @param f the function to map non-zero values
    * @param M the monoid instance for `M`
    * @param Me the equality instance for `M`
    * @return `self` if empty, otherwise `f(self)`
    */
  def mapNZ(f: M => M)(implicit M: Monoid[M], Me: Equal[M]): M =
    M.onNotEmpty(self)(f(self))

}

object MonoidOps extends ToMonoidOps

trait ToMonoidOps {
  import language.implicitConversions

  @inline implicit final def ToMonoidOps[M: Monoid](m: M): MonoidOps[M] =
    new MonoidOps[M](m)
}
