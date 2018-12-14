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

import scalaz._

package object scaloi {
  type Attempt[A] = Throwable \/ A

  /** A (Scala) partial function.
    *
    * Basically a [[Kleisli]] arrow over [[Option]], but using exceptions.
    */
  type =∂>[-A, +R] = PartialFunction[A, R]

  /**
    * A [[scaloi.ClassMap]] with no lower bound.
    * @tparam U the upper bound of the types of values in this [[scaloi.ClassMap]]
    */
  type ClassMap0[U] = ClassMap[U, Nothing]

  import MultiMap._
  /** For a fixed `K` and `V`, `MultiMap[K, V]` is a monoid. */
  implicit def MultiMapMonoid[K, V]: Monoid[MultiMap[K, V]] =
    new Monoid[MultiMap[K, V]] {
      def zero: MultiMap[K, V] = Map.empty

      def append(f1: MultiMap[K, V], f2: => MultiMap[K, V]) = f1 combine f2
    }
}
