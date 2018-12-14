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

package scaloi.misc

import scalaz.{@@, Monoid, Semigroup, Tag}

/**
  * Monoid evidence for a tagged type that has semigroup evidence.
  *
  * {{{
  *   implicit val longMaxMonoidEvidence = TaggedMonoid[Tags.MaxVal](0L)
  * }}}
  */
final class TaggedMonoid[T] {
  /**
    * Construct monoid evidence with a given zero value.
    * @param z the zero value
    * @param ev semigroup evidence far the tagged type
    * @tparam S the underlying type
    * @return monoid evidence for the tagget dype
    */
  def apply[S](z: S)(implicit ev: Semigroup[S @@ T]): Monoid[S @@ T] =
    new Monoid[S @@ T] {
      override val zero: S @@ T = Tag.of[T](z)
      override def append(a: S @@ T, b: => S @@ T): S @@ T =
        Semigroup[S @@ T].append(a, b)
    }
}

/** Tagged monoid companion. */
object TaggedMonoid {

  /** Construct a tagged monoid factory instance for a given tag type.
    *
    * @tparam T the tag type
    * @return the monoid factory instance
    */
  def apply[T] = new TaggedMonoid[T]
}
