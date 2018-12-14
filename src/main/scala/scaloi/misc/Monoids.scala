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

import scalaz.syntax.tag._
import scalaz.{@@, Monoid, Tag, \/}

/**
  * Miscellaneous monoids.
  */
object Monoids {

  /**
    * A tag for disjunctions desirous of a monoid instance which selects
    * the first left value, if any exist.
    */
  sealed trait FailFast

  /** @see [[FailFast]] */
  final val FailFast = Tag.of[FailFast]

  /**
    * Monoid instance for disjunctions that fails fast on the first left. Contrast
    * with the standard DisjunctionMonoid which requires a Semigroup left type that
    * accumulates results.
    *
    * @tparam A the left type
    * @tparam B the right type
    * @return a fail-fast monoid instance
    */
  implicit def failFastDisjunctionMonoid[A, B: Monoid]: Monoid[(A \/ B) @@ FailFast] =
    new Monoid[(A \/ B) @@ FailFast] {
      type ABFF = (A \/ B) @@ FailFast

      override def zero: ABFF = Monoid[B].zero.rightFF[A]

      override def append(l: ABFF, r: => ABFF): ABFF =
        FailFast {
          for (lv <- l.unwrap; rv <- r.unwrap) yield {
            Monoid[B].append(lv, rv)
          }
        }
    }

  implicit class FailFastMonoidSyntaxOps[T](private val self: T) extends AnyVal {
    import scalaz.syntax.either._

    /**
      * Wrap a value in the left side of a fail-fast disjunction.
      * @tparam B the right-hand type
      * @return the value, on the left
      */
    @inline def leftFF[B]: (T \/ B) @@ FailFast = FailFast(self.left)

    /**
      * Wrap a value in the right side of a fail-fast disjunction.
      * @tparam A the left-hand type
      * @return the value, on the right
      */
    @inline def rightFF[A]: (A \/ T) @@ FailFast = FailFast(self.right)
  }

  /**
    * A [[Monoid]] for [[Map]]s which relies on [[scala.collection.MapLike.++]]
    * to append maps. This means that it prefers the key-value pairs from the
    * right-side map over those from the left-side map.
    *
    * This is morally equivalent to tagging the values with [[scalaz.Tags.LastVal]].
    */
  implicit def rightBiasMapMonoid[K, V]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V]                                    = Map.empty
    override def append(f1: Map[K, V], f2: => Map[K, V]): Map[K, V] = f1 ++ f2
  }
}
